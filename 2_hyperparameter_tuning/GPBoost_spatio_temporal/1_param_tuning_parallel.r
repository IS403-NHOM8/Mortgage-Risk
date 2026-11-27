if (!nzchar(Sys.getenv("RSTUDIO"))) {
  project_lib <- paste0(
    "D:/UIT_DOCUMENTS/NAM_3/",
    "PHÂN TÍCH DỮ LIỆU KINH DOANH-20260704T155950Z-3-001/",
    "Đồ án/Mortgage-Risk/renv/library/windows/R-4.5/x86_64-w64-mingw32"
  )
  if (!dir.exists(project_lib)) {
    stop("Không tìm thấy project library: ", project_lib)
  }
  .libPaths(c(project_lib, .libPaths()))
  if (!requireNamespace("gpboost", quietly = TRUE)) {
    stop("Vẫn không thấy gpboost sau khi prepend. ",
         "Có thể symlink broken — chạy renv::restore() trong RStudio.")
  }
}

required_packages <- c("gpboost", "data.table")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing package: ", pkg)
  }
}
library(gpboost)
library(data.table)
if (packageVersion("gpboost") < "1.8.0") {
  stop("Script này yêu cầu gpboost >= 1.8.0.")
}

project_dir <- paste0(
  "D:/UIT_DOCUMENTS/NAM_3/",
  "PHÂN TÍCH DỮ LIỆU KINH DOANH-20260704T155950Z-3-001/",
  "Đồ án/Mortgage-Risk"
)
data_file <- file.path(project_dir, "1_data_set", "data", "processed", "freddie_mac", "data_window_2022.rds")
tuning_root <- file.path(project_dir, "2_hyperparameter_tuning", "GPBoost_spatio_temporal")
results_root <- file.path(project_dir, "results", "GPBoost_spatio_temporal")
dir.create(tuning_root, recursive = TRUE, showWarnings = FALSE)
dir.create(results_root, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(data_file)) {
  stop("Không tìm thấy file dữ liệu: ", data_file)
}

DATA_MIN_YEAR <- 2011L
DATA_MAX_YEAR <- 2022L

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  requested_year <- as.integer(args[1])
  if (is.na(requested_year) || !(requested_year %in% 2016:2021)) {
    stop("Validation year không hợp lệ: ", args[1], ". Phải là một trong 2016:2021.")
  }
  validation_years <- requested_year
} else {
  validation_years <- 2016:2021
}

INITIAL_ROUNDS <- 500L
EXTENDED_ROUNDS <- 1000L
EARLY_STOPPING <- 20L
EXTENSION_THRESHOLD <- 480L
NUM_THREADS <- 8L
RANDOM_SEED <- 1L
GP_COV_FUNCTION <- "matern_space_time"
GP_COV_SHAPE <- 1.5
GP_APPROX <- "vecchia"
GP_VECCHIA_ORDERING <- "time_random_space"
GP_NUM_NEIGHBORS <- 20L
GP_MATRIX_INVERSION <- "iterative"
GP_VECCHIA_PRED_TYPE <- "latent_order_obs_first_cond_obs_only"
GP_NUM_NEIGHBORS_PRED <- 20L
RUN_TAG <- "paper_aligned_v2"

grid_formals <- names(formals(gpboost::gpb.grid.search.tune.parameters))
required_grid_args <- c("param_grid", "params", "num_try_random", "train_gp_model_cov_pars",
                        "verbose_eval", "gp_model", "nrounds", "early_stopping_rounds", "folds", "eval")
missing_grid_args <- setdiff(required_grid_args, grid_formals)
if (length(missing_grid_args) > 0) {
  stop("GPBoost API không có các arguments cần thiết: ", paste(missing_grid_args, collapse = ", "))
}

data_2022 <- readRDS(data_file)
required_names <- c("Y", "X", "ID", "AUX")
if (!all(required_names %in% names(data_2022))) {
  stop("Cấu trúc RDS không đúng. Thành phần hiện có: ", paste(names(data_2022), collapse = ", "))
}
Y0 <- as.integer(data_2022$Y)
X0 <- as.data.frame(data_2022$X)
ID0 <- data_2022$ID
AUX0 <- data_2022$AUX
if (nrow(X0) != length(Y0)) stop("Số dòng X không khớp số phần tử Y.")
if (nrow(X0) != length(ID0)) stop("Số dòng X không khớp số phần tử ID.")
if (anyNA(Y0)) stop("Biến Y có NA.")
if (!all(Y0 %in% c(0L, 1L))) stop("Y phải chỉ chứa 0 và 1.")
if (!isTRUE(AUX0$cumulative)) stop("Dữ liệu không được xác nhận là cumulative.")
if (!"date_versioning" %in% names(X0)) stop("Không tìm thấy date_versioning.")
date_values <- as.Date(X0$date_versioning)
if (anyNA(date_values)) stop("date_versioning có giá trị không hợp lệ.")
year0 <- as.integer(format(date_values, "%Y"))

keep_idx <- which(year0 >= DATA_MIN_YEAR & year0 <= DATA_MAX_YEAR)
if (length(keep_idx) == 0) stop("Không có quan sát trong giai đoạn 2011-2022.")
X0 <- X0[keep_idx, , drop = FALSE]
Y0 <- Y0[keep_idx]
ID0 <- ID0[keep_idx]
year0 <- year0[keep_idx]
rm(data_2022)
gc()

if (nrow(X0) != length(Y0) || nrow(X0) != length(ID0)) {
  stop("Kích thước dữ liệu không đồng nhất sau khi lọc.")
}
available_years <- sort(unique(year0))
if (!all(validation_years %in% available_years)) {
  stop("Thiếu validation year: ", paste(setdiff(validation_years, available_years), collapse = ", "))
}

feature_cols <- setdiff(names(X0), c("current_upb", "date_versioning"))
cat_vars <- c("occupancy", "nr_units", "loan_purpose", "first_time_homebuyer", "MSA", "number_of_borrowers")
required_coords <- c("X_centroid", "Y_centroid")
if (!all(required_coords %in% names(X0))) stop("Thiếu X_centroid hoặc Y_centroid.")
if (!all(cat_vars %in% feature_cols)) stop("Một hoặc nhiều biến categorical không có trong feature_cols.")
factor_levels <- list(
  occupancy = c("P", "I", "S"),
  nr_units = c("1", "2", "3", "4"),
  loan_purpose = c("P", "N", "C"),
  first_time_homebuyer = c("0", "1"),
  MSA = c("0", "1"),
  number_of_borrowers = c("1", "2")
)

get_mode <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

fit_imputer <- function(X, train_idx, feature_cols, cat_vars) {
  imputer <- list(numeric = list(), categorical = list())
  for (v in feature_cols) {
    if (v %in% cat_vars) {
      mode_value <- get_mode(X[[v]][train_idx])
      if (is.na(mode_value)) stop("Không xác định được mode cho biến: ", v)
      imputer$categorical[[v]] <- mode_value
    } else {
      raw_values <- X[[v]][train_idx]
      values <- suppressWarnings(as.numeric(as.character(raw_values)))
      invalid_conversion <- (!is.na(raw_values) & is.na(values))
      if (any(invalid_conversion)) stop("Không thể chuyển dữ liệu sang numeric: ", v)
      med <- median(values, na.rm = TRUE)
      if (!is.finite(med)) stop("Không tính được median cho biến: ", v)
      imputer$numeric[[v]] <- med
    }
  }
  imputer
}

apply_imputer <- function(X, imputer, feature_cols, cat_vars, factor_levels) {
  X_out <- X[, feature_cols, drop = FALSE]
  for (v in feature_cols) {
    if (v %in% cat_vars) {
      x <- as.character(X_out[[v]])
      x[is.na(x) | trimws(x) == ""] <- imputer$categorical[[v]]
      levels_v <- factor_levels[[v]]
      unknown_values <- setdiff(unique(x[!is.na(x)]), levels_v)
      if (length(unknown_values) > 0) {
        stop("Giá trị categorical ngoài levels của ", v, ": ", paste(unknown_values, collapse = ", "))
      }
      X_out[[v]] <- factor(x, levels = levels_v)
    } else {
      x <- suppressWarnings(as.numeric(as.character(X_out[[v]])))
      x[is.na(x)] <- imputer$numeric[[v]]
      X_out[[v]] <- x
    }
  }
  X_out
}

param_grid <- expand.grid(
  learning_rate = c(10, 1, 0.1),
  min_data_in_leaf = c(10, 100, 1000),
  max_depth = c(2, 3, 5, 10),
  lambda_l2 = c(0, 1, 10),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
param_grid$combination_id <- seq_len(nrow(param_grid))
stopifnot(nrow(param_grid) == 108L)

save_atomic <- function(object, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  temp_file <- tempfile(pattern = "checkpoint_", tmpdir = dirname(file_path), fileext = ".rds")
  saveRDS(object, temp_file)
  if (file.exists(file_path)) file.remove(file_path)
  success <- file.rename(temp_file, file_path)
  if (!success) {
    success <- file.copy(temp_file, file_path, overwrite = TRUE)
    file.remove(temp_file)
  }
  if (!success) stop("Không thể lưu checkpoint: ", file_path)
  invisible(TRUE)
}

read_checkpoint_checked <- function(file_path, expected_signature) {
  obj <- readRDS(file_path)
  if (is.null(obj$run_signature) || !identical(obj$run_signature, expected_signature)) {
    stop("Checkpoint không khớp cấu hình hiện tại: ", file_path, "\nKhông tự động ghi đè checkpoint này.")
  }
  obj
}

run_one_combination <- function(param_row, sample_idx, X_design, Y0, coords_fold,
                                valid_idx, nrounds, valid_year, pass_name, run_signature) {
  combination_id <- as.integer(param_row$combination_id)
  dtrain <- gpb.Dataset(data = X_design[sample_idx, , drop = FALSE], label = Y0[sample_idx])
  valid_local_idx <- match(valid_idx, sample_idx)
  if (anyNA(valid_local_idx)) stop("Không xác định được validation local index.")
  gp_model <- GPModel(
    gp_coords = coords_fold[sample_idx, , drop = FALSE],
    cov_function = GP_COV_FUNCTION,
    cov_fct_shape = GP_COV_SHAPE,
    likelihood = "bernoulli_logit",
    gp_approx = GP_APPROX,
    vecchia_ordering = GP_VECCHIA_ORDERING,
    num_neighbors = GP_NUM_NEIGHBORS,
    matrix_inversion_method = GP_MATRIX_INVERSION
  )
  gp_model$set_prediction_data(vecchia_pred_type = GP_VECCHIA_PRED_TYPE,
                               num_neighbors_pred = GP_NUM_NEIGHBORS_PRED)
  gp_model$set_optim_params(list(trace = TRUE, seed_rand_vec_trace = 1))
  one_param_grid <- list(
    learning_rate = as.numeric(param_row$learning_rate),
    min_data_in_leaf = as.integer(param_row$min_data_in_leaf),
    max_depth = as.integer(param_row$max_depth),
    lambda_l2 = as.numeric(param_row$lambda_l2)
  )
  other_params <- list(num_leaves = 2^10, objective = "binary",
                       feature_pre_filter = FALSE, verbosity = -1L, num_threads = NUM_THREADS)
  set.seed(RANDOM_SEED)
  result <- gpb.grid.search.tune.parameters(
    param_grid = one_param_grid, params = other_params, num_try_random = NULL,
    train_gp_model_cov_pars = TRUE, data = dtrain, gp_model = gp_model,
    folds = list(valid_local_idx), nrounds = as.integer(nrounds),
    early_stopping_rounds = EARLY_STOPPING, eval = "auc", verbose_eval = 2
  )
  best_iter <- as.integer(result$best_iter[1])
  best_score <- as.numeric(result$best_score[1])
  if (length(best_iter) != 1 || is.na(best_iter) ||
      length(best_score) != 1 || !is.finite(best_score)) {
    stop("Không lấy được best_iter hoặc best_score hợp lệ.\n",
         "Validation year: ", valid_year, "\nCombination: ", combination_id)
  }
  output <- list(
    run_signature = run_signature, combination_id = combination_id,
    validation_year = valid_year, pass = pass_name,
    parameters = list(
      learning_rate = one_param_grid$learning_rate,
      min_data_in_leaf = one_param_grid$min_data_in_leaf,
      max_depth = one_param_grid$max_depth,
      lambda_l2 = one_param_grid$lambda_l2,
      num_leaves = other_params$num_leaves
    ),
    best_iter = best_iter, best_score = best_score,
    nrounds = as.integer(nrounds), result = result
  )
  rm(dtrain, gp_model, result)
  gc()
  output
}

run_validation_fold <- function(valid_year) {
  train_idx <- which(year0 < valid_year)
  valid_idx <- which(year0 == valid_year)
  if (length(train_idx) == 0 || length(valid_idx) == 0) {
    stop("Train hoặc validation rỗng tại năm ", valid_year)
  }
  imputer <- fit_imputer(X = X0, train_idx = train_idx,
                         feature_cols = feature_cols, cat_vars = cat_vars)
  sample_idx <- c(train_idx, valid_idx)
  X_fold <- apply_imputer(X = X0[sample_idx, , drop = FALSE], imputer = imputer,
                          feature_cols = feature_cols, cat_vars = cat_vars,
                          factor_levels = factor_levels)
  X_design <- model.matrix(~ ., data = X_fold)
  X_design <- X_design[, colnames(X_design) != "(Intercept)", drop = FALSE]
  if (anyNA(X_design) || any(!is.finite(X_design))) {
    stop("X_design còn NA hoặc Inf tại validation year ", valid_year)
  }
  coords_fold <- cbind(
    time = as.numeric(year0[sample_idx]),
    X_centroid = as.numeric(X0$X_centroid[sample_idx]),
    Y_centroid = as.numeric(X0$Y_centroid[sample_idx])
  )
  if (anyNA(coords_fold) || any(!is.finite(coords_fold))) {
    stop("GP coordinates có NA hoặc Inf tại năm ", valid_year)
  }
  fold_dir <- file.path(tuning_root, paste0("data_", valid_year))
  initial_dir <- file.path(fold_dir, paste0("grid108_checkpoints_", RUN_TAG))
  extended_dir <- file.path(fold_dir, paste0("grid108_extended_1000_checkpoints_", RUN_TAG))
  dir.create(initial_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(extended_dir, recursive = TRUE, showWarnings = FALSE)
  run_signature <- paste(
    RUN_TAG, "gpboost_1.8.0", valid_year, DATA_MIN_YEAR, DATA_MAX_YEAR,
    INITIAL_ROUNDS, EXTENDED_ROUNDS, EARLY_STOPPING, EXTENSION_THRESHOLD,
    GP_COV_FUNCTION, GP_COV_SHAPE, GP_APPROX, GP_VECCHIA_ORDERING,
    GP_NUM_NEIGHBORS, GP_MATRIX_INVERSION, GP_VECCHIA_PRED_TYPE,
    GP_NUM_NEIGHBORS_PRED, sep = "|"
  )

  for (i in seq_len(nrow(param_grid))) {
    checkpoint_file <- file.path(initial_dir, sprintf("combination_%03d.rds", i))
    if (file.exists(checkpoint_file)) {
      existing <- read_checkpoint_checked(checkpoint_file, run_signature)
      if (existing$nrounds != INITIAL_ROUNDS) stop("Checkpoint initial không đúng nrounds: ", checkpoint_file)
      rm(existing); next
    }
    param_row <- param_grid[i, , drop = FALSE]
    result <- run_one_combination(
      param_row = param_row, sample_idx = sample_idx, X_design = X_design,
      Y0 = Y0, coords_fold = coords_fold, valid_idx = valid_idx,
      nrounds = INITIAL_ROUNDS, valid_year = valid_year,
      pass_name = "initial_500", run_signature = run_signature
    )
    save_atomic(result, checkpoint_file)
    rm(result); gc()
  }

  initial_rows <- vector("list", nrow(param_grid))
  for (i in seq_len(nrow(param_grid))) {
    checkpoint_file <- file.path(initial_dir, sprintf("combination_%03d.rds", i))
    if (!file.exists(checkpoint_file)) stop("Thiếu initial checkpoint: ", checkpoint_file)
    obj <- read_checkpoint_checked(checkpoint_file, run_signature)
    initial_rows[[i]] <- data.frame(
      combination_id = obj$combination_id,
      learning_rate = obj$parameters$learning_rate,
      min_data_in_leaf = obj$parameters$min_data_in_leaf,
      max_depth = obj$parameters$max_depth,
      lambda_l2 = obj$parameters$lambda_l2,
      best_iter = obj$best_iter, best_score = obj$best_score,
      pass = obj$pass, stringsAsFactors = FALSE
    )
    rm(obj)
  }
  initial_table <- rbindlist(initial_rows, fill = TRUE)
  rm(initial_rows); gc()
  fwrite(initial_table, file.path(fold_dir,
    paste0("tuning_initial_500_", valid_year, "_", RUN_TAG, ".csv")))

  extension_ids <- initial_table$combination_id[
    !is.na(initial_table$best_iter) & initial_table$best_iter >= EXTENSION_THRESHOLD]

  if (length(extension_ids) > 0) {
    for (i in extension_ids) {
      checkpoint_file <- file.path(extended_dir, sprintf("combination_%03d.rds", i))
      if (file.exists(checkpoint_file)) {
        existing <- read_checkpoint_checked(checkpoint_file, run_signature)
        if (existing$nrounds != EXTENDED_ROUNDS) stop("Extended checkpoint không đúng nrounds: ", checkpoint_file)
        rm(existing); next
      }
      param_row <- param_grid[param_grid$combination_id == i, , drop = FALSE]
      result <- run_one_combination(
        param_row = param_row, sample_idx = sample_idx, X_design = X_design,
        Y0 = Y0, coords_fold = coords_fold, valid_idx = valid_idx,
        nrounds = EXTENDED_ROUNDS, valid_year = valid_year,
        pass_name = "extended_1000", run_signature = run_signature
      )
      save_atomic(result, checkpoint_file)
      rm(result); gc()
    }
  }

  final_rows <- vector("list", nrow(param_grid))
  for (i in seq_len(nrow(param_grid))) {
    initial_file <- file.path(initial_dir, sprintf("combination_%03d.rds", i))
    initial_obj <- read_checkpoint_checked(initial_file, run_signature)
    initial_best_iter <- initial_obj$best_iter
    initial_best_score <- initial_obj$best_score
    initial_parameters <- initial_obj$parameters
    rm(initial_obj)
    extended_file <- file.path(extended_dir, sprintf("combination_%03d.rds", i))
    if (i %in% extension_ids) {
      if (!file.exists(extended_file)) stop("Thiếu extended checkpoint: ", extended_file)
      final_obj <- read_checkpoint_checked(extended_file, run_signature)
      final_best_iter <- final_obj$best_iter
      final_best_score <- final_obj$best_score
      final_pass <- "extended_1000"
      rm(final_obj)
    } else {
      final_best_iter <- initial_best_iter
      final_best_score <- initial_best_score
      final_pass <- "initial_500"
    }
    final_rows[[i]] <- data.frame(
      combination_id = i,
      learning_rate = initial_parameters$learning_rate,
      min_data_in_leaf = initial_parameters$min_data_in_leaf,
      max_depth = initial_parameters$max_depth,
      lambda_l2 = initial_parameters$lambda_l2,
      initial_best_iter = initial_best_iter,
      initial_best_score = initial_best_score,
      final_best_iter = final_best_iter,
      final_best_score = final_best_score,
      final_pass = final_pass, stringsAsFactors = FALSE
    )
    rm(initial_parameters)
  }
  final_table <- rbindlist(final_rows, fill = TRUE)
  rm(final_rows); gc()
  setorder(final_table, -final_best_score)

  fwrite(final_table, file.path(fold_dir,
    paste0("tuning_all_combinations_108_", valid_year, "_", RUN_TAG, ".csv")))
  best_result <- final_table[1]
  fwrite(best_result, file.path(fold_dir,
    paste0("tuning_summary_108_", valid_year, "_", RUN_TAG, ".csv")))

  fold_output <- list(
    run_signature = run_signature, validation_year = valid_year,
    train_years = sort(unique(year0[train_idx])),
    validation_n = length(valid_idx),
    initial_rounds = INITIAL_ROUNDS, extended_rounds = EXTENDED_ROUNDS,
    extension_threshold = EXTENSION_THRESHOLD,
    early_stopping_rounds = EARLY_STOPPING,
    extension_ids = extension_ids,
    all_results = final_table, best_result = best_result
  )
  final_fold_file <- file.path(fold_dir,
    paste0("tuning_result_108_", valid_year, "_", RUN_TAG, ".rds"))
  save_atomic(fold_output, final_fold_file)

  rm(X_fold, X_design, coords_fold, imputer, initial_table)
  gc()
  fold_output
}

all_fold_results <- list()
for (valid_year in validation_years) {
  fold_dir <- file.path(tuning_root, paste0("data_", valid_year))
  final_fold_file <- file.path(fold_dir,
    paste0("tuning_result_108_", valid_year, "_", RUN_TAG, ".rds"))
  if (file.exists(final_fold_file)) {
    existing_result <- tryCatch(readRDS(final_fold_file), error = function(e) NULL)
    if (!is.null(existing_result) &&
        identical(existing_result$validation_year, valid_year) &&
        !is.null(existing_result$run_signature) &&
        !is.null(existing_result$all_results) &&
        nrow(existing_result$all_results) == 108L) {
      all_fold_results[[as.character(valid_year)]] <- existing_result
      rm(existing_result); next
    }
    rm(existing_result)
  }
  fold_result <- run_validation_fold(valid_year)
  all_fold_results[[as.character(valid_year)]] <- fold_result
  rm(fold_result); gc()
}

combined_best <- rbindlist(
  lapply(all_fold_results, function(x) as.data.frame(x$best_result)),
  fill = TRUE)
setorder(combined_best, validation_year)
combined_file <- file.path(results_root,
  paste0("tuning_summary_108_2016_2021_500_to_1000_", RUN_TAG, ".csv"))
fwrite(combined_best, combined_file)