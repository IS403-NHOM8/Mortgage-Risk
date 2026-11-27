library(gpboost)
library(data.table)

set.seed(1)

DATA_FILE   <- file.path("1_data_set", "data", "processed", "freddie_mac", "data_window_2022.rds")
TUNING_ROOT <- file.path("2_hyperparameter_tuning", "GPBoost_spatio_temporal")

if (!file.exists(DATA_FILE)) {
  stop("Không tìm thấy: ", DATA_FILE,
       "\nHãy đảm bảo bạn chạy script từ thư mục gốc project (hoặc dùng renv::restore()).")
}
if (packageVersion("gpboost") < "1.8.0") stop("Yêu cầu gpboost >= 1.8.0")
dir.create(TUNING_ROOT, recursive = TRUE, showWarnings = FALSE)

DATA_MIN_YEAR       <- 2011L
DATA_MAX_YEAR       <- 2022L
INITIAL_ROUNDS      <- 500L
EXTENDED_ROUNDS     <- 1000L
EARLY_STOPPING      <- 20L
EXTENSION_THRESHOLD <- 480L
NUM_THREADS         <- 8L
RANDOM_SEED         <- 1L
RUN_TAG             <- "paper_aligned_v2"

GP_COV_FUNCTION     <- "matern_space_time"
GP_COV_SHAPE        <- 1.5
GP_APPROX           <- "vecchia"
GP_VECCHIA_ORDERING <- "time_random_space"
GP_NUM_NEIGHBORS    <- 20L
GP_MATRIX_INVERSION <- "iterative"
GP_VECCHIA_PRED_TYPE<- "latent_order_obs_first_cond_obs_only"
GP_NUM_NEIGHBORS_PRED <- 20L

args <- commandArgs(trailingOnly = TRUE)
validation_years <- if (length(args) >= 1) {
  y <- as.integer(args[1])
  if (is.na(y) || !(y %in% 2016:2021)) stop("Validation year không hợp lệ: ", args[1])
  y
} else 2016:2021

data_2022 <- readRDS(DATA_FILE)
Y0   <- as.integer(data_2022$Y)
X0   <- as.data.frame(data_2022$X)
ID0  <- data_2022$ID
AUX0 <- data_2022$AUX
rm(data_2022)

if (nrow(X0) != length(Y0) || nrow(X0) != length(ID0)) stop("X/Y/ID không khớp số dòng")
if (anyNA(Y0) || !all(Y0 %in% c(0L, 1L))) stop("Y phải là 0/1, không có NA")
if (!isTRUE(AUX0$cumulative)) stop("Data phải là cumulative")
if (!"date_versioning" %in% names(X0)) stop("Thiếu date_versioning")

date_values <- as.Date(X0$date_versioning)
if (anyNA(date_values)) stop("date_versioning có NA")
year0 <- as.integer(format(date_values, "%Y"))

keep <- which(year0 >= DATA_MIN_YEAR & year0 <= DATA_MAX_YEAR)
if (!length(keep)) stop("Không có quan sát trong ", DATA_MIN_YEAR, "-", DATA_MAX_YEAR)
X0 <- X0[keep, , drop = FALSE]; Y0 <- Y0[keep]; ID0 <- ID0[keep]; year0 <- year0[keep]
gc()

if (!all(validation_years %in% year0)) {
  stop("Thiếu validation years: ", paste(setdiff(validation_years, unique(year0)), collapse = ", "))
}

feature_cols <- setdiff(names(X0), c("current_upb", "date_versioning"))
cat_vars <- c("occupancy", "nr_units", "loan_purpose",
              "first_time_homebuyer", "MSA", "number_of_borrowers")
factor_levels <- list(
  occupancy            = c("P", "I", "S"),
  nr_units             = c("1", "2", "3", "4"),
  loan_purpose         = c("P", "N", "C"),
  first_time_homebuyer = c("0", "1"),
  MSA                  = c("0", "1"),
  number_of_borrowers  = c("1", "2")
)
stopifnot(all(c("X_centroid", "Y_centroid") %in% names(X0)))
stopifnot(all(cat_vars %in% feature_cols))

get_mode <- function(x) {
  x <- as.character(x); x <- x[!is.na(x) & trimws(x) != ""]
  if (!length(x)) return(NA_character_)
  names(which.max(table(x)))
}

fit_imputer <- function(X, train_idx) {
  imp <- list(numeric = list(), categorical = list())
  for (v in feature_cols) {
    if (v %in% cat_vars) {
      m <- get_mode(X[[v]][train_idx])
      if (is.na(m)) stop("Không tính được mode cho: ", v)
      imp$categorical[[v]] <- m
    } else {
      vals <- suppressWarnings(as.numeric(as.character(X[[v]][train_idx])))
      if (all(is.na(vals))) stop("Không convert được sang numeric: ", v)
      imp$numeric[[v]] <- median(vals, na.rm = TRUE)
    }
  }
  imp
}

apply_imputer <- function(X, imp) {
  X_out <- X[, feature_cols, drop = FALSE]
  for (v in feature_cols) {
    if (v %in% cat_vars) {
      x <- as.character(X_out[[v]])
      x[is.na(x) | trimws(x) == ""] <- imp$categorical[[v]]
      unknown <- setdiff(unique(x[!is.na(x)]), factor_levels[[v]])
      if (length(unknown)) stop("Level lạ ở ", v, ": ", paste(unknown, collapse = ", "))
      X_out[[v]] <- factor(x, levels = factor_levels[[v]])
    } else {
      x <- suppressWarnings(as.numeric(as.character(X_out[[v]])))
      x[is.na(x)] <- imp$numeric[[v]]
      X_out[[v]] <- x
    }
  }
  X_out
}

save_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("ckpt_", dirname(path), ".rds")
  saveRDS(object, tmp)
  if (file.exists(path)) file.remove(path)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE); file.remove(tmp)
  }
  invisible(TRUE)
}

read_checkpoint <- function(path, signature) {
  obj <- readRDS(path)
  if (!identical(obj$run_signature, signature))
    stop("Checkpoint signature lệch config hiện tại: ", path,
         "\nNếu cố ý đổi config, hãy đổi RUN_TAG.")
  obj
}

param_grid <- expand.grid(
  learning_rate    = c(10, 1, 0.1),
  min_data_in_leaf = c(10, 100, 1000),
  max_depth        = c(2, 3, 5, 10),
  lambda_l2        = c(0, 1, 10),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
param_grid$combination_id <- seq_len(nrow(param_grid))
stopifnot(nrow(param_grid) == 108L)

run_combo <- function(param_row, sample_idx, X_design, Y0, coords_fold,
                      valid_idx, nrounds, valid_year, pass_name, run_signature) {
  dtrain <- gpb.Dataset(data = X_design[sample_idx, , drop = FALSE], label = Y0[sample_idx])
  local_valid <- match(valid_idx, sample_idx)

  gp_model <- GPModel(
    gp_coords               = coords_fold[sample_idx, , drop = FALSE],
    cov_function            = GP_COV_FUNCTION,
    cov_fct_shape           = GP_COV_SHAPE,
    likelihood              = "bernoulli_logit",
    gp_approx               = GP_APPROX,
    vecchia_ordering        = GP_VECCHIA_ORDERING,
    num_neighbors           = GP_NUM_NEIGHBORS,
    matrix_inversion_method = GP_MATRIX_INVERSION
  )
  gp_model$set_prediction_data(vecchia_pred_type = GP_VECCHIA_PRED_TYPE,
                               num_neighbors_pred = GP_NUM_NEIGHBORS_PRED)
  gp_model$set_optim_params(list(trace = TRUE, seed_rand_vec_trace = 1))

  one_grid <- list(
    learning_rate    = as.numeric(param_row$learning_rate),
    min_data_in_leaf = as.integer(param_row$min_data_in_leaf),
    max_depth        = as.integer(param_row$max_depth),
    lambda_l2        = as.numeric(param_row$lambda_l2)
  )
  other_params <- list(num_leaves = 2^10, objective = "binary",
                       feature_pre_filter = FALSE, verbosity = -1L,
                       num_threads = NUM_THREADS)

  set.seed(RANDOM_SEED)
  res <- gpb.grid.search.tune.parameters(
    param_grid = one_grid, params = other_params, num_try_random = NULL,
    train_gp_model_cov_pars = TRUE, data = dtrain, gp_model = gp_model,
    folds = list(local_valid), nrounds = as.integer(nrounds),
    early_stopping_rounds = EARLY_STOPPING, eval = "auc", verbose_eval = 2
  )

  best_iter  <- as.integer(res$best_iter[1])
  best_score <- as.numeric(res$best_score[1])
  if (is.na(best_iter) || !is.finite(best_score))
    stop("best_iter / best_score không hợp lệ tại combo ", param_row$combination_id)

  out <- list(
    run_signature = run_signature,
    combination_id = as.integer(param_row$combination_id),
    validation_year = valid_year, pass = pass_name,
    parameters = one_grid,
    best_iter = best_iter, best_score = best_score,
    nrounds = as.integer(nrounds)
  )
  rm(dtrain, gp_model, res); gc()
  out
}

run_fold <- function(valid_year) {
  train_idx <- which(year0 < valid_year)
  valid_idx <- which(year0 == valid_year)
  if (!length(train_idx) || !length(valid_idx))
    stop("Train hoặc validation rỗng tại năm ", valid_year)

  imputer <- fit_imputer(X0, train_idx)
  sample_idx <- c(train_idx, valid_idx)
  X_fold   <- apply_imputer(X0[sample_idx, , drop = FALSE], imputer)
  X_design <- model.matrix(~ ., data = X_fold)
  X_design <- X_design[, colnames(X_design) != "(Intercept)", drop = FALSE]
  if (anyNA(X_design) || any(!is.finite(X_design)))
    stop("X_design còn NA/Inf tại year ", valid_year)

  coords_fold <- cbind(
    time       = as.numeric(year0[sample_idx]),
    X_centroid = as.numeric(X0$X_centroid[sample_idx]),
    Y_centroid = as.numeric(X0$Y_centroid[sample_idx])
  )
  if (anyNA(coords_fold) || any(!is.finite(coords_fold)))
    stop("Coords có NA/Inf tại year ", valid_year)

  fold_dir <- file.path(TUNING_ROOT, paste0("data_", valid_year))
  dir.create(fold_dir, recursive = TRUE, showWarnings = FALSE)

  run_signature <- paste(
    RUN_TAG, "gpboost_1.8.0", valid_year, DATA_MIN_YEAR, DATA_MAX_YEAR,
    INITIAL_ROUNDS, EXTENDED_ROUNDS, EARLY_STOPPING, EXTENSION_THRESHOLD,
    GP_COV_FUNCTION, GP_COV_SHAPE, GP_APPROX, GP_VECCHIA_ORDERING,
    GP_NUM_NEIGHBORS, GP_MATRIX_INVERSION, GP_VECCHIA_PRED_TYPE,
    GP_NUM_NEIGHBORS_PRED, sep = "|"
  )

  ckpt_path <- function(i) file.path(fold_dir, sprintf("combination_%03d.rds", i))

  for (i in seq_len(nrow(param_grid))) {
    ckpt <- ckpt_path(i)
    if (file.exists(ckpt)) {
      existing <- read_checkpoint(ckpt, run_signature)
      if (!existing$nrounds %in% c(INITIAL_ROUNDS, EXTENDED_ROUNDS))
        stop("nrounds không hợp lệ trong ", ckpt)
      rm(existing); next
    }
    res <- run_combo(param_grid[i, , drop = FALSE], sample_idx, X_design, Y0,
                     coords_fold, valid_idx, INITIAL_ROUNDS, valid_year,
                     "initial_500", run_signature)
    save_atomic(res, ckpt); rm(res); gc()
  }

  extension_ids <- integer(0)
  for (i in seq_len(nrow(param_grid))) {
    o <- read_checkpoint(ckpt_path(i), run_signature)
    if (identical(o$pass, "initial_500") && o$best_iter >= EXTENSION_THRESHOLD) {
      extension_ids <- c(extension_ids, i)
    }
    rm(o)
  }

  for (i in extension_ids) {
    res <- run_combo(param_grid[i, , drop = FALSE], sample_idx, X_design, Y0,
                     coords_fold, valid_idx, EXTENDED_ROUNDS, valid_year,
                     "extended_1000", run_signature)
    save_atomic(res, ckpt_path(i)); rm(res); gc()
  }

  rm(X_fold, X_design, coords_fold, imputer); gc()
  invisible(NULL)
}

for (valid_year in validation_years) {
  run_fold(valid_year)
  gc()
}