# 1. Cau hinh va khoi tao luoi 80 to hop
root <- file.path("2_hyperparameter_tuning", "Random_Forest")
data_dir <- file.path("1_data_set", "data", "processed", "freddie_mac")
val_years <- 2016:2021
version <- "rf_tuning_expanding_median_mode_2026_v4"
scope <- "retrospective_exploratory"
ranger_ver <- as.character(utils::packageVersion("ranger"))

grid <- expand.grid(
  num.trees  = c(500L, 1000L),
  mtry       = c(5L, 8L, 11L, 15L, 20L),
  min.bucket = c(5L, 20L),
  max.depth  = c(10L, 15L),
  splitrule  = c("gini", "hellinger"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# 2. Doc va kiem tra tinh hop le cua 80 ket qua tung nam
scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
read_year <- function(y) {
  folder <- file.path(root, paste0("data_", y))
  input <- file.path(data_dir, sprintf("data_window_%d.rds", y))
  if (!file.exists(input)) stop("Missing input: ", input, call. = FALSE)
  md5 <- unname(tools::md5sum(input))
  if (is.na(md5)) stop("Cannot hash input: ", input, call. = FALSE)

  results <- lapply(seq_len(nrow(grid)), function(i) {
    path <- file.path(folder, sprintf("param_comb_%02d.rds", i))
    fail <- function(msg) stop(path, ": ", msg, call. = FALSE)
    if (!file.exists(path)) fail("missing tuning result")
    r <- tryCatch(readRDS(path), error = function(e) fail(conditionMessage(e)))
    if (!is.list(r) || !identical(r$val_year, y) ||
        !identical(r$i_param, i) || !identical(r$pipeline_version, version) ||
        !identical(r$grid_size, nrow(grid)) || !identical(r$evaluation_scope, scope) ||
        !identical(r$ranger_version, ranger_ver) ||
        !identical(r$input_md5, md5) || !identical(r$error, "")) {
      fail("wrong year, combination, pipeline/ranger/data version or failed job")
    }
    if (!is.data.frame(r$params) || nrow(r$params) != 1L ||
        !identical(r$params, grid[i, , drop = FALSE])) {
      fail("grid parameters do not match the v4 tuning grid")
    }
    p <- r$n_features_encoded
    if (!scalar(p) || p < 1L || p != floor(p) ||
        length(r$encoded_feature_names) != p ||
        r$params$mtry < 1L || r$params$mtry > p) {
      fail("invalid feature schema or mtry")
    }
    if (!scalar(r$val_auc) || r$val_auc < 0 || r$val_auc > 1 ||
        !scalar(r$val_brier) || r$val_brier < 0 || r$val_brier > 1 ||
        !scalar(r$val_logloss) || r$val_logloss < 0 ||
        !scalar(r$elapsed_sec) || r$elapsed_sec < 0) fail("invalid metrics")
    r
  })

  fields <- c("train_years", "n_train", "n_val", "defaults_train", "defaults_val",
              "n_features_encoded", "encoded_feature_names", "excluded_features",
              "numeric_medians", "categorical_modes", "preprocessing_rule",
              "seed", "library", "ranger_version", "grid_size",
              "evaluation_scope", "selection_metric")
  ref <- results[[1L]]
  if (!all(fields %in% names(ref)) ||
      !identical(ref$train_years, 2011:(y - 1L)) ||
      !identical(ref$preprocessing_rule, "median_numeric_mode_categorical_train_only_v3") ||
      !identical(ref$seed, 1L) || !identical(ref$library, "ranger") ||
      !all(vapply(results, function(r) identical(r[fields], ref[fields]), logical(1)))) {
    stop("Inconsistent split, features or preprocessing in validation year ", y, call. = FALSE)
  }

  # 3. Xep hang theo ROC-AUC, Brier, so cay va chi so to hop
  summary <- do.call(rbind, lapply(results, function(r) {
    data.frame(val_year = y, test_year = y + 1L, i_param = r$i_param,
               r$params, val_auc = r$val_auc, val_logloss = r$val_logloss,
               val_brier = r$val_brier, elapsed_sec = r$elapsed_sec,
               stringsAsFactors = FALSE)
  }))
  summary <- summary[order(-summary$val_auc, summary$val_brier,
                           summary$num.trees, summary$i_param), ]
  rownames(summary) <- NULL
  best <- results[[summary$i_param[1L]]]
  best$tie_breakers <- c("lower Brier", "fewer trees", "smaller combination index")
  cat(sprintf("Checked %d: %d/%d valid | Best #%02d (trees=%d, mtry=%d, mb=%d, depth=%d, split=%s) | AUC=%.6f\n",
              y, length(results), nrow(grid), best$i_param, best$params$num.trees,
              best$params$mtry, best$params$min.bucket, best$params$max.depth,
              best$params$splitrule, best$val_auc))
  list(year = y, folder = folder, summary = summary, best = best)
}

selected <- lapply(val_years, read_year)

# 4. Luu ket qua tot nhat theo tung nam
for (s in selected) {
  y <- s$year
  write.csv(s$summary, file.path(s$folder, sprintf("tuning_summary_%d.csv", y)), row.names = FALSE)
  saveRDS(s$best, file.path(s$folder, sprintf("best_param_comb_%d.rds", y)))
  saveRDS(s$best$params, file.path(s$folder, sprintf("best_params_%d.rds", y)))
}

# 5. Luu bang tong hop 480 to hop va 6 nam
all_results <- do.call(rbind, lapply(selected, `[[`, "summary"))
best_all <- do.call(rbind, lapply(selected, function(s) s$summary[1L, , drop = FALSE]))
rownames(all_results) <- NULL
rownames(best_all) <- NULL
write.csv(all_results, file.path(root, "tuning_summary_all_years.csv"), row.names = FALSE)
write.csv(best_all, file.path(root, "best_params_all_years.csv"), row.names = FALSE)
print(best_all, row.names = FALSE)
cat(sprintf("Done: %d combinations checked; %d annual best configurations saved (%s).\n",
            nrow(all_results), nrow(best_all), scope))
