data_dir <- file.path("3_training_and_prediction", "GPBoost_spatial", "data")
pred_years <- 2017:2022

all_predictions <- list()
all_parameters <- list()

for (y in pred_years) {
  pred_file <- file.path(data_dir, paste0("prediction_", y, ".rds"))
  param_file <- file.path(data_dir, paste0("parameters_", y - 1, ".rds"))

  if (!file.exists(pred_file)) {
    warning(sprintf("Missing prediction file for year %d: %s", y, pred_file))
    next
  }

  y_pred <- readRDS(pred_file)
  slot_name <- paste0("pred_year_", y)
  all_predictions[[slot_name]] <- y_pred[[slot_name]]

  if (file.exists(param_file)) {
    all_parameters[[as.character(y - 1)]] <- readRDS(param_file)
  }
}

out_combined_pred <- file.path(data_dir, "prediction_GPBoost_spatial.rds")
out_combined_param <- file.path(data_dir, "parameters_GPBoost_spatial.rds")

saveRDS(all_predictions, out_combined_pred)
if (length(all_parameters) > 0) {
  saveRDS(do.call(rbind, all_parameters), out_combined_param)
}
