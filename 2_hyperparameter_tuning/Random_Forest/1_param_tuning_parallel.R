# 1. Nhan tham so va khoi tao luoi 80 to hop
set.seed(1L)
for (pkg in c("ranger", "pROC")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install package: ", pkg)
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 2:3 || (length(args) == 3L && args[3] != "--force")) {
  stop("Usage: Rscript 1_param_tuning_parallel.R <val_year> <comb_nr> [--force]")
}
val_year <- suppressWarnings(as.integer(args[1]))
comb_nr <- suppressWarnings(as.integer(args[2]))
force <- length(args) == 3L
if (is.na(val_year) || !val_year %in% 2016:2021) stop("val_year must be 2016..2021")

grid <- expand.grid(
  num.trees  = c(500L, 1000L),
  mtry       = c(5L, 8L, 11L, 15L, 20L),
  min.bucket = c(5L, 20L),
  max.depth  = c(10L, 15L),
  splitrule  = c("gini", "hellinger"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
if (is.na(comb_nr) || !comb_nr %in% seq_len(nrow(grid))) stop("comb_nr must be 1..80")
param <- grid[comb_nr, , drop = FALSE]

input <- file.path("1_data_set", "data", "processed", "freddie_mac",
                   sprintf("data_window_%d.rds", val_year))
out_dir <- file.path("2_hyperparameter_tuning", "Random_Forest", paste0("data_", val_year))
out_file <- file.path(out_dir, sprintf("param_comb_%02d.rds", comb_nr))
if (!file.exists(input)) stop("Missing data file: ", input)
input_md5 <- unname(tools::md5sum(input))
if (is.na(input_md5)) stop("Cannot hash input file")
version <- "rf_tuning_expanding_median_mode_2026_v4"
ranger_ver <- as.character(utils::packageVersion("ranger"))

# 2. Kiem tra ket qua cu de bo qua neu da hoan thanh
if (file.exists(out_file) && !force) {
  old <- tryCatch(readRDS(out_file), error = function(e) NULL)
  scalar_ok <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  valid <- is.list(old) && identical(old$val_year, val_year) &&
    identical(old$i_param, comb_nr) && identical(old$input_md5, input_md5) &&
    identical(old$pipeline_version, version) && identical(old$ranger_version, ranger_ver) &&
    identical(old$seed, 1L) && identical(old$grid_size, nrow(grid)) &&
    identical(old$error, "") &&
    scalar_ok(old$val_auc) && old$val_auc >= 0 && old$val_auc <= 1 &&
    scalar_ok(old$val_logloss) && old$val_logloss >= 0 &&
    scalar_ok(old$val_brier) && old$val_brier >= 0 && old$val_brier <= 1 &&
    is.data.frame(old$params) && identical(old$params, param)
  if (valid) {
    cat("Already completed: ", out_file, " (AUC=", old$val_auc, ")\n", sep = "")
    quit(save = "no", status = 0L)
  }
}

# 3. Doc du lieu va chia Expanding Window
d <- readRDS(input)
if (!is.list(d) || !all(c("X", "Y") %in% names(d)) ||
    !is.data.frame(d$X) || !"date_versioning" %in% names(d$X) ||
    !inherits(d$X$date_versioning, "Date")) stop("Invalid RDS structure (X, Y, date_versioning)")
if (nrow(d$X) != length(d$Y) || anyNA(d$Y) || !all(d$Y %in% c(0L, 1L))) {
  stop("Invalid binary labels Y")
}
years <- as.integer(format(d$X$date_versioning, "%Y"))
if (anyNA(years) || any(years > val_year)) stop("Invalid/future dates: check data window")
train <- which(years < val_year)
val <- which(years == val_year)
if (!length(train) || !length(val) ||
    length(unique(d$Y[train])) != 2L || length(unique(d$Y[val])) != 2L) {
  stop("Train/validation must both exist and contain both classes")
}

# 4. Dien khuyet Median/Mode va tao co thieu tren Inner Train
excluded <- c("current_upb", "date_versioning")
features <- setdiff(names(d$X), excluded)
if (!length(features)) stop("No predictors")
x_train <- d$X[train, features, drop = FALSE]
x_val <- d$X[val, features, drop = FALSE]
y_train <- factor(d$Y[train], levels = c(0L, 1L), labels = c("0", "1"))
y_val <- d$Y[val]

CATEGORY_LEVELS <- list(
  occupancy = c("P", "S", "I"),
  nr_units = as.character(1:4),
  loan_purpose = c("C", "N", "P"),
  first_time_homebuyer = c("0", "1"),
  MSA = c("0", "1"),
  number_of_borrowers = c("1", "2")
)
train_mode <- function(values, allowed) {
  observed <- values[!is.na(values) & values %in% allowed]
  if (!length(observed)) return(allowed[1L])
  allowed[which.max(tabulate(match(observed, allowed), nbins = length(allowed)))]
}

numeric_medians <- list()
categorical_modes <- list()
for (nm in features) {
  categorical <- nm %in% names(CATEGORY_LEVELS) ||
    is.character(x_train[[nm]]) || is.factor(x_train[[nm]]) ||
    is.logical(x_train[[nm]])

  if (categorical) {
    a <- trimws(as.character(x_train[[nm]]))
    b <- trimws(as.character(x_val[[nm]]))
    a[!is.na(a) & !nzchar(a)] <- NA_character_
    b[!is.na(b) & !nzchar(b)] <- NA_character_
    allowed <- if (nm %in% names(CATEGORY_LEVELS)) CATEGORY_LEVELS[[nm]] else sort(unique(a[!is.na(a)]))
    if (!length(allowed)) allowed <- "Missing"
    a[!a %in% allowed] <- NA_character_
    b[!b %in% allowed] <- NA_character_
    fill <- train_mode(a, allowed)
    a[is.na(a)] <- fill
    b[is.na(b)] <- fill
    if (length(allowed) == 1L) {
      reserved <- "__RF_UNUSED_LEVEL__"
      while (reserved %in% allowed) reserved <- paste0(reserved, "_")
      allowed <- c(allowed, reserved)
    }
    x_train[[nm]] <- factor(a, levels = allowed)
    x_val[[nm]] <- factor(b, levels = allowed)
    categorical_modes[[nm]] <- fill
  } else {
    a <- suppressWarnings(as.numeric(x_train[[nm]]))
    b <- suppressWarnings(as.numeric(x_val[[nm]]))
    a[!is.finite(a)] <- NA_real_
    b[!is.finite(b)] <- NA_real_
    miss_train <- is.na(a)
    if (any(miss_train)) {
      indicator <- paste0(nm, "_missing")
      if (indicator %in% names(x_train)) stop("Feature collision: ", indicator)
      x_train[[indicator]] <- as.integer(miss_train)
      x_val[[indicator]] <- as.integer(is.na(b))
    }
    med <- if (all(miss_train)) 0 else stats::median(a, na.rm = TRUE)
    a[is.na(a)] <- med
    b[is.na(b)] <- med
    x_train[[nm]] <- a
    x_val[[nm]] <- b
    numeric_medians[[nm]] <- med
  }
}

X_train <- stats::model.matrix(~ . - 1, data = x_train)
X_val <- stats::model.matrix(~ . - 1, data = x_val)
if (!identical(colnames(X_train), colnames(X_val)) ||
    !all(is.finite(X_train)) || !all(is.finite(X_val))) stop("Invalid design matrices")
p <- ncol(X_train)
if (p == 0L) stop("Empty design matrix")
if (param$mtry < 1L || param$mtry > p) {
  stop(sprintf("Invalid mtry=%d for %d encoded features", param$mtry, p))
}
if (param$splitrule == "hellinger" && nlevels(y_train) != 2L) {
  stop("Hellinger splitrule requires binary factor response")
}

threads <- suppressWarnings(as.integer(Sys.getenv("RF_NUM_THREADS", "2")))
if (is.na(threads) || threads < 1L) stop("RF_NUM_THREADS must be >= 1")
cat(sprintf("Validation %d | grid %02d/%d | trees=%d mtry=%d mb=%d depth=%d split=%s | train=%d, val=%d\n",
            val_year, comb_nr, nrow(grid), param$num.trees, param$mtry,
            param$min.bucket, param$max.depth, param$splitrule, length(train), length(val)))

# 5. Huan luyen Probability Random Forest va danh gia tren Validation
start <- proc.time()[[3]]
fit <- tryCatch({
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
  probs <- predict(forest, data = X_val, num.threads = threads)$predictions
  if (is.null(dim(probs)) || !"1" %in% colnames(probs)) stop("Missing P(Y=1)")
  p1 <- as.numeric(probs[, "1"])
  if (length(p1) != length(y_val) || anyNA(p1) ||
      !all(is.finite(p1)) || any(p1 < 0 | p1 > 1)) stop("Invalid probabilities")
  auc <- as.numeric(pROC::auc(pROC::roc(y_val, p1, levels = c(0, 1), direction = "<", quiet = TRUE)))
  q <- pmin(pmax(p1, 1e-15), 1 - 1e-15)
  list(val_auc = auc,
       val_logloss = -mean(y_val * log(q) + (1 - y_val) * log(1 - q)),
       val_brier = mean((p1 - y_val)^2), error = "")
}, error = function(e) list(val_auc = NA_real_, val_logloss = NA_real_,
                            val_brier = NA_real_, error = conditionMessage(e)))

result <- c(list(val_year = val_year, i_param = comb_nr, params = param), fit,
            list(elapsed_sec = proc.time()[[3]] - start, seed = 1L, library = "ranger",
                 ranger_version = ranger_ver, num_threads = threads,
                 data_file = input, input_md5 = input_md5,
                 pipeline_version = version, grid_size = nrow(grid),
                 grid_design = "rf_v4_80_gini_hellinger",
                 evaluation_scope = "retrospective_exploratory",
                 train_years = sort(unique(years[train])), validation_year = val_year,
                 n_train = length(train), n_val = length(val),
                 defaults_train = sum(d$Y[train] == 1L), defaults_val = sum(d$Y[val] == 1L),
                 n_features_encoded = p, encoded_feature_names = colnames(X_train),
                 excluded_features = excluded, categorical_modes = categorical_modes,
                 numeric_medians = numeric_medians,
                 preprocessing_rule = "median_numeric_mode_categorical_train_only_v3",
                 selection_metric = "ROC-AUC (probability Y=1)"))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tmp <- tempfile(pattern = "rf_result_", tmpdir = out_dir, fileext = ".rds")
saveRDS(result, tmp)
if (file.exists(out_file)) unlink(out_file)
if (!file.rename(tmp, out_file)) stop("Cannot save: ", out_file)
cat(sprintf("Saved %s | AUC=%s | %.1f sec\n", out_file, result$val_auc, result$elapsed_sec))
if (nzchar(result$error)) stop("Tuning failed: ", result$error)
