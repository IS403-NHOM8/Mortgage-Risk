pred_years <- 2017:2022
n_years    <- length(pred_years)

out_dir <- file.path("3_training_and_prediction", "GPBoost_spatio_temporal", "data")
if (!dir.exists(out_dir)) stop("Output dir not found: ", out_dir)

par_cnames <- c("year", "model",
                "GP_var", "GP_range_time", "GP_range_space",
                "INIT_GP_var", "INIT_GP_range_time", "INIT_GP_range_space",
                "final_neg_log_likelihood", "time_training")

parameters       <- vector("list", n_years)
predictions      <- vector("list", n_years)
imputation_stats <- vector("list", n_years)
names(predictions)      <- paste0("pred_year_", pred_years)
names(imputation_stats) <- paste0("pred_year_", pred_years)

for (i in seq_along(pred_years)) {
  pred_year <- pred_years[i]
  val_year  <- pred_year - 1

  par_path  <- file.path(out_dir, paste0("parameters_",       val_year,  ".rds"))
  pred_path <- file.path(out_dir, paste0("prediction_",       pred_year, ".rds"))
  imp_path  <- file.path(out_dir, paste0("imputation_stats_", pred_year, ".rds"))

  missing <- c(par_path, pred_path, imp_path)[
    !file.exists(c(par_path, pred_path, imp_path))]
  if (length(missing))
    stop("Missing files for ", pred_year, ":\n", paste(missing, collapse = "\n"))

  y_par <- readRDS(par_path)
  if (as.integer(y_par$year[1]) != val_year)
    stop("Year mismatch in ", par_path)
  parameters[[i]] <- y_par[1, par_cnames, drop = FALSE]

  y_pred    <- readRDS(pred_path)
  pred_name <- paste0("pred_year_", pred_year)
  if (is.null(y_pred[[pred_name]]))
    stop("Bad prediction structure: ", pred_path)
  predictions[[i]] <- y_pred[[pred_name]]

  imputation_stats[[i]] <- readRDS(imp_path)
}

parameters <- do.call(rbind, parameters)
rownames(parameters) <- NULL

saveRDS(parameters,       file.path(out_dir, "parameters_GPBoost_spatio_temporal.rds"))
saveRDS(predictions,      file.path(out_dir, "prediction_GPBoost_spatio_temporal.rds"))
saveRDS(imputation_stats, file.path(out_dir, "imputation_stats_GPBoost_spatio_temporal.rds"))

cat("Combined", n_years, "years (", min(pred_years), "-", max(pred_years), ")\n")
