val_years <- 2016:2021
tuning_dir <- file.path("2_hyperparameter_tuning", "GPBoost_spatial")
all_best_list <- list()

for (val_year in val_years) {
  year_folder <- file.path(tuning_dir, paste0("data_", val_year))
  if (!dir.exists(year_folder)) {
    warning(sprintf("Missing folder: %s", year_folder))
    next
  }

  files <- list.files(path = year_folder, pattern = "^param_comb_.*\\.rds$", full.names = TRUE)
  files <- files[!grepl("hyperpars_", basename(files))]
  if (length(files) == 0) {
    warning(sprintf("No tuning files in %s", year_folder))
    next
  }

  param_combs <- do.call(rbind, lapply(files, readRDS))

  full_csv_path <- file.path(year_folder, paste0("all_108_param_combs_", val_year, ".csv"))
  write.csv(param_combs, full_csv_path, row.names = FALSE)

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
  final_summary <- do.call(rbind, all_best_list)
  summary_csv <- file.path(tuning_dir, "summary_best_hyperparameters_all_years.csv")
  write.csv(final_summary, summary_csv, row.names = FALSE)
}
