library(data.table)

val_years <- 2016:2021
tuning_dir <- file.path("2_hyperparameter_tuning", "GPBoost_spatio_temporal")
all_best_list <- list()

read_combo_file <- function(f) {
  obj <- readRDS(f)
  if (is.data.frame(obj)) return(as.data.table(obj))
  if (is.list(obj) && !is.null(obj$parameters)) {
    return(data.table(
      year             = obj$validation_year,
      combination_id   = obj$combination_id,
      learning_rate    = obj$parameters$learning_rate,
      min_data_in_leaf = obj$parameters$min_data_in_leaf,
      max_depth        = obj$parameters$max_depth,
      lambda_l2        = obj$parameters$lambda_l2,
      best_iter        = obj$best_iter,
      best_score       = obj$best_score,
      nrounds          = obj$nrounds,
      pass             = obj$pass
    ))
  }
  stop("Unknown format: ", f)
}

for (val_year in val_years) {
  year_folder <- file.path(tuning_dir, paste0("data_", val_year))
  if (!dir.exists(year_folder)) {
    warning(sprintf("Missing folder: %s", year_folder))
    next
  }

  files <- list.files(
    path = year_folder,
    pattern = "^(combination|param_comb)_\\d+\\.rds$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    warning(sprintf("No tuning files in %s", year_folder))
    next
  }

  param_combs <- rbindlist(lapply(files, read_combo_file), fill = TRUE)

  full_csv_path <- file.path(year_folder, paste0("all_108_param_combs_", val_year, ".csv"))
  fwrite(param_combs, full_csv_path)

  i_best <- which.max(param_combs$best_score)
  best_comb <- param_combs[i_best, ]
  best_comb$test_year <- val_year + 1

  out_hyper_year <- file.path(year_folder, paste0("hyperpars_", val_year, ".rds"))
  out_hyper_root <- file.path(tuning_dir, paste0("hyperpars_", val_year, ".rds"))

  saveRDS(best_comb, out_hyper_year)
  saveRDS(best_comb, out_hyper_root)

  all_best_list[[as.character(val_year)]] <- best_comb
}

if (length(all_best_list) > 0) {
  final_summary <- rbindlist(all_best_list, fill = TRUE)
  summary_csv <- file.path(tuning_dir, "summary_best_hyperparameters_all_years.csv")
  fwrite(final_summary, summary_csv)
}