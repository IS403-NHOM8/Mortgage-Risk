# 1. Cau hinh va kiem tra duong dan
if (!requireNamespace("ranger", quietly = TRUE)) stop("Install package: ranger")
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2 || (length(args) == 2L && args[2L] != "--force"))
  stop("Usage: Rscript train_predict_parallel.R <2017..2022> [--force]")
pred_year <- suppressWarnings(as.integer(args[1L]))
if (is.na(pred_year) || !pred_year %in% 2017:2022) stop("Invalid prediction year (expected 2017..2022)")
force <- length(args) == 2L
val_year <- pred_year - 1L
version <- "rf_tuning_expanding_median_mode_2026_v4"
train_version <- "rf_final_annual_median_mode_v4"
scope <- "retrospective_exploratory"
ranger_ver <- as.character(utils::packageVersion("ranger"))
threads <- suppressWarnings(as.integer(Sys.getenv("RF_NUM_THREADS", "2")))
if (is.na(threads) || threads < 1L) stop("RF_NUM_THREADS must be >= 1")
set.seed(1L)

root <- file.path("1_data_set", "data", "processed", "freddie_mac")
input <- file.path(root, sprintf("data_window_%d.rds", pred_year))
val_input <- file.path(root, sprintf("data_window_%d.rds", val_year))
tuning <- file.path("2_hyperparameter_tuning", "Random_Forest", paste0("data_", val_year))
param_path <- file.path(tuning, sprintf("best_params_%d.rds", val_year))
best_path <- file.path(tuning, sprintf("best_param_comb_%d.rds", val_year))
out <- file.path("3_training_and_prediction", "Random_Forest", "data")
paths <- setNames(file.path(out, sprintf(c("rf_model_%d.rds", "predictions_%d.rds",
                                           "predictions_%d.csv", "complete_%d.rds"), pred_year)),
                  c("model", "pred", "csv", "complete"))
inputs <- c(input, val_input, param_path, best_path)
if (!all(file.exists(inputs))) stop("Missing: ", paste(inputs[!file.exists(inputs)], collapse = ", "))
hashes <- unname(tools::md5sum(inputs))
if (anyNA(hashes)) stop("Cannot hash input files")

# 2. Doc sieu tham so tot nhat va kiem tra resume
param <- readRDS(param_path)
best <- readRDS(best_path)
excluded <- c("current_upb", "date_versioning")
if (!is.list(best) || !identical(best$val_year, val_year) ||
    !identical(best$pipeline_version, version) || !identical(best$grid_size, 80L) ||
    !identical(best$evaluation_scope, scope) || !identical(best$ranger_version, ranger_ver) ||
    !identical(best$error, "") || !identical(best$input_md5, hashes[2L]) ||
    !identical(best$excluded_features, excluded) ||
    !is.numeric(best$val_auc) || length(best$val_auc) != 1L || !is.finite(best$val_auc) ||
    !identical(best$params, param) || !is.data.frame(param) || nrow(param) != 1L ||
    !identical(names(param), c("num.trees", "mtry", "min.bucket", "max.depth", "splitrule")))
  stop("Invalid/stale annual best parameters; rerun Stage 2 selection")
if (!isTRUE(param$num.trees %in% c(500L, 1000L) &&
            param$mtry %in% c(5L, 8L, 11L, 15L, 20L) &&
            param$min.bucket %in% c(5L, 20L) &&
            param$max.depth %in% c(10L, 15L) &&
            param$splitrule %in% c("gini", "hellinger")))
  stop("Parameters outside the RF v4 grid")
signature <- list(version = train_version, tuning_version = version,
                  evaluation_scope = scope, hashes = hashes,
                  year = pred_year, ranger_version = ranger_ver, threads = threads)
if (!force && all(file.exists(paths))) {
  old <- tryCatch(readRDS(paths[["complete"]]), error = function(e) NULL)
  if (is.list(old) && isTRUE(old$complete) && identical(old$signature, signature)) {
    cat("Already completed:", pred_year, "\n"); quit(save = "no", status = 0L)
  }
}
if (!force && any(file.exists(paths))) stop("Existing incomplete/stale outputs; inspect and use --force")
cat(sprintf("Test %d | Validation %d | Best #%02d (trees=%d, mtry=%d, mb=%d, depth=%d, split=%s) | Val AUC %.6f\n",
            pred_year, val_year, best$i_param, param$num.trees, param$mtry,
            param$min.bucket, param$max.depth, param$splitrule, best$val_auc))

# 3. Doc du lieu va chia Final Train / Test
start <- proc.time()[["elapsed"]]
d <- readRDS(input)
if (!is.list(d) || !is.data.frame(d$X) || !inherits(d$X$date_versioning, "Date") ||
    length(d$Y) != nrow(d$X) || anyNA(d$Y) || !all(d$Y %in% c(0L, 1L)))
  stop("Invalid X, date_versioning or binary labels Y")
years <- as.integer(format(d$X$date_versioning, "%Y"))
train_years <- 2011:val_year
if (anyNA(years) || !identical(sort(unique(years)), 2011:pred_year)) stop("Invalid expanding window")
train <- which(years < pred_year)
test <- which(years == pred_year)
if (length(unique(d$Y[train])) != 2L) stop("Final Train must contain both classes")
if (!isTRUE(length(train) == best$n_train + best$n_val) ||
    !isTRUE(sum(d$Y[train] == 1L) == best$defaults_train + best$defaults_val))
  stop("Historical rows/counts differ from selected tuning window; audit input data")
features <- setdiff(names(d$X), excluded)
if (!length(features) || anyDuplicated(names(d$X))) stop("Invalid predictors")
x_train <- d$X[train, features, drop = FALSE]
x_test <- d$X[test, features, drop = FALSE]
y_train <- factor(d$Y[train], levels = c(0L, 1L), labels = c("0", "1"))
y_test <- as.integer(as.character(d$Y[test]))
ids <- d$ID
if (is.data.frame(ids) && nrow(ids) == nrow(d$X)) {
  ids <- ids[test, , drop = FALSE]
} else if (is.atomic(ids) && length(ids) == nrow(d$X)) {
  ids <- ids[test]
} else if (!is.null(ids)) stop("ID cannot be aligned to rows")
loan_id <- if (is.data.frame(ids) && ncol(ids) == 1L) as.character(ids[[1L]]) else
  if (is.atomic(ids) && length(ids) == length(test)) as.character(ids) else rep(NA_character_, length(test))
rm(d, years); invisible(gc())

# 4. Tien xu ly du lieu chi tinh tren Final Train
CATEGORY_LEVELS <- list(occupancy = c("P", "S", "I"), nr_units = as.character(1:4),
                        loan_purpose = c("C", "N", "P"), first_time_homebuyer = c("0", "1"),
                        MSA = c("0", "1"), number_of_borrowers = c("1", "2"))
recipe <- list(raw_features = features, excluded_features = excluded, numeric_medians = list(),
               categorical_modes = list(), categorical_levels = list(), missing_indicators = character(),
               preprocessing_rule = "median_numeric_mode_categorical_train_only_v3")
for (nm in features) {
  a <- x_train[[nm]]; b <- x_test[[nm]]
  if (nm %in% names(CATEGORY_LEVELS) || is.character(a) || is.factor(a) || is.logical(a)) {
    a <- trimws(as.character(a)); b <- trimws(as.character(b))
    a[!is.na(a) & !nzchar(a)] <- NA_character_
    b[!is.na(b) & !nzchar(b)] <- NA_character_
    allowed <- if (nm %in% names(CATEGORY_LEVELS)) CATEGORY_LEVELS[[nm]] else sort(unique(a[!is.na(a)]))
    if (!length(allowed)) allowed <- "Missing"
    a[!a %in% allowed] <- NA_character_; b[!b %in% allowed] <- NA_character_
    fill <- allowed[which.max(tabulate(match(a[!is.na(a)], allowed), nbins = length(allowed)))]
    a[is.na(a)] <- fill; b[is.na(b)] <- fill
    if (length(allowed) == 1L) {
      reserved <- "__RF_UNUSED_LEVEL__"
      while (reserved %in% allowed) reserved <- paste0(reserved, "_")
      allowed <- c(allowed, reserved)
    }
    x_train[[nm]] <- factor(a, levels = allowed); x_test[[nm]] <- factor(b, levels = allowed)
    recipe$categorical_modes[[nm]] <- fill; recipe$categorical_levels[[nm]] <- allowed
  } else {
    a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
    a[!is.finite(a)] <- NA_real_; b[!is.finite(b)] <- NA_real_
    miss <- is.na(a)
    if (any(miss)) {
      ind <- paste0(nm, "_missing")
      if (ind %in% names(x_train)) stop("Feature collision: ", ind)
      x_train[[ind]] <- as.integer(miss); x_test[[ind]] <- as.integer(is.na(b))
      recipe$missing_indicators <- c(recipe$missing_indicators, ind)
    }
    med <- if (all(miss)) 0 else stats::median(a, na.rm = TRUE)
    a[is.na(a)] <- med; b[is.na(b)] <- med
    x_train[[nm]] <- a; x_test[[nm]] <- b; recipe$numeric_medians[[nm]] <- med
  }
}
X_train <- stats::model.matrix(~ . - 1, data = x_train)
X_test <- stats::model.matrix(~ . - 1, data = x_test)
rm(x_train, x_test, a, b); invisible(gc())
if (nrow(X_train) != length(train) || nrow(X_test) != length(test) ||
    !identical(colnames(X_train), colnames(X_test)) || !ncol(X_train) ||
    any(!is.finite(X_train)) || any(!is.finite(X_test))) stop("Invalid design matrices")
recipe$encoded_feature_names <- colnames(X_train)
p <- ncol(X_train)
if (!identical(recipe$encoded_feature_names, best$encoded_feature_names))
  cat("Encoded schema differs from Inner Train; verifying mtry against Final Train features.\n")
if (param$mtry < 1L || param$mtry > p) {
  stop(sprintf("Invalid mtry=%d for %d Final Train encoded features", param$mtry, p))
}
if (param$splitrule == "hellinger" && nlevels(y_train) != 2L) {
  stop("Hellinger splitrule requires binary factor response")
}

# 5. Huan luyen Probability Random Forest voi 5 sieu tham so da chon
cat(sprintf("Train=%d | Test=%d | Features=%d | Trees=%d | mtry=%d | mb=%d | depth=%d | split=%s\n",
            length(train), length(test), p, param$num.trees, param$mtry,
            param$min.bucket, param$max.depth, param$splitrule))
fit_start <- proc.time()[["elapsed"]]
forest <- ranger::ranger(
  x = X_train,
  y = y_train,
  num.trees = param$num.trees,
  mtry = param$mtry,
  min.bucket = param$min.bucket,
  max.depth = param$max.depth,
  splitrule = param$splitrule,
  min.node.size = 1L,
  probability = TRUE,
  replace = TRUE,
  sample.fraction = 1,
  write.forest = TRUE,
  oob.error = FALSE,
  importance = "none",
  num.threads = threads,
  seed = 1L,
  na.action = "na.fail",
  verbose = FALSE
)
fit_sec <- proc.time()[["elapsed"]] - fit_start
rm(X_train, y_train); invisible(gc())

# 6. Du bao xac suat vo no theo tung khoi du lieu
p1 <- numeric(length(test))
for (left in seq.int(1L, length(test), by = 50000L)) {
  idx <- left:min(left + 49999L, length(test))
  probs <- predict(forest, data = X_test[idx, , drop = FALSE], num.threads = threads)$predictions
  if (!is.matrix(probs) || !"1" %in% colnames(probs) || nrow(probs) != length(idx)) stop("Invalid prediction shape")
  p1[idx] <- probs[, "1"]
}
if (any(!is.finite(p1)) || any(p1 < 0 | p1 > 1)) stop("Invalid probabilities")
rm(X_test, probs); invisible(gc())
elapsed <- proc.time()[["elapsed"]] - start
pred_table <- data.frame(row_index = test, loan_id = loan_id, prediction_year = pred_year,
                         p_default = p1, y_true = y_test, stringsAsFactors = FALSE)

# 7. Luu mo hinh, ket qua du bao va file xac nhan
model_bundle <- list(model = forest, preprocessing = recipe, params = param,
                     pipeline_version = version, evaluation_scope = scope,
                     validation_selection = list(year = val_year, combination = best$i_param, val_auc = best$val_auc,
                                                 tuned_params = best$params), prediction_year = pred_year,
                     trained_years = train_years, data_file = input, package = "ranger",
                     package_version = ranger_ver, seed = 1L, signature = signature)
pred_bundle <- list(prediction_year = pred_year, validation_year = val_year, diagnostic_only = FALSE,
                    pipeline_version = version, evaluation_scope = scope,
                    predictions = pred_table, ID = ids, params = param, training_years = train_years,
                    n_train = length(train), n_prediction = length(test), elapsed_sec = elapsed,
                    time_training = fit_sec, warning = "", signature = signature)
save_output <- function(object, path, csv = FALSE) {
  tmp <- tempfile("rf_", tmpdir = out)
  on.exit(unlink(tmp), add = TRUE)
  if (csv) write.csv(object, tmp, row.names = FALSE, na = "") else saveRDS(object, tmp)
  if (file.exists(path) && unlink(path) != 0L) stop("Cannot replace: ", path)
  if (!file.rename(tmp, path)) stop("Cannot save: ", path)
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if (file.exists(paths[["complete"]]) && unlink(paths[["complete"]]) != 0L) stop("Cannot remove old completion marker")
save_output(model_bundle, paths[["model"]])
save_output(pred_bundle, paths[["pred"]])
save_output(pred_table, paths[["csv"]], csv = TRUE)
save_output(list(complete = TRUE, prediction_year = pred_year, diagnostic_only = FALSE,
                 pipeline_version = version, evaluation_scope = scope,
                 n_train = length(train), n_prediction = length(test), elapsed_sec = elapsed,
                 data_file = input, signature = signature), paths[["complete"]])
cat(sprintf("DONE %d | Training %.1f sec | Total %.1f sec (%s)\n", pred_year, fit_sec, elapsed, scope))
