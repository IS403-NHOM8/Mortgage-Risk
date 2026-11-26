# 1. Nhan tham so nam va kiem tra thu muc du lieu
args <- commandArgs(trailingOnly = TRUE)
data_dir <- file.path("3_training_and_prediction", "Random_Forest", "data")
if (!dir.exists(data_dir)) stop("Missing Stage 3 output directory: ", data_dir)

if (length(args) == 0L) {
  paths <- list.files(data_dir, pattern = "^complete_[0-9]{4}\\.rds$", full.names = TRUE)
  years <- suppressWarnings(as.integer(sub("^complete_([0-9]{4})\\.rds$", "\\1", basename(paths))))
  years <- sort(unique(years[!is.na(years) & years >= 2016L]))
} else {
  years <- suppressWarnings(as.integer(args))
  if (anyNA(years) || any(years < 2016L) || anyDuplicated(years)) {
    stop("Specify distinct independent test years >= 2016 only. Year 2015 is validation.")
  }
  years <- sort(years)
}
if (length(years) == 0L) stop("No completed independent prediction years >= 2016 found")

# 2. Doc va kiem tra ket qua du bao tung nam
results <- lapply(years, function(year) {
  complete_path <- file.path(data_dir, sprintf("complete_%d.rds", year))
  pred_path <- file.path(data_dir, sprintf("predictions_%d.rds", year))
  if (!file.exists(complete_path) || !file.exists(pred_path)) {
    stop("Missing prediction/complete marker for year ", year)
  }
  marker <- readRDS(complete_path)
  pred <- readRDS(pred_path)
  if (!is.list(marker) || !isTRUE(marker$complete) || isTRUE(marker$diagnostic_only) ||
      !identical(as.integer(marker$prediction_year), as.integer(year)) ||
      !is.list(pred) || isTRUE(pred$diagnostic_only) ||
      !identical(as.integer(pred$prediction_year), as.integer(year))) {
    stop("Invalid final-test output for year ", year)
  }
  expected <- c("row_index", "loan_id", "prediction_year", "p_default", "y_true")
  if (!is.data.frame(pred$predictions) || !identical(names(pred$predictions), expected)) {
    stop("Unexpected predictions schema for year ", year)
  }
  pred$predictions
})
# 3. Gop va luu ket qua tat ca cac nam
combined <- do.call(rbind, results)
rownames(combined) <- NULL
out_rds <- file.path(data_dir, "combined_predictions.rds")
out_csv <- file.path(data_dir, "combined_predictions.csv")
saveRDS(list(years = years, predictions = combined), out_rds)
utils::write.csv(combined, out_csv, row.names = FALSE, na = "")
cat(sprintf("Combined %d independent prediction year(s), %d loan-year rows\n",
            length(years), nrow(combined)))
cat("Saved: ", out_rds, "\nSaved: ", out_csv, "\n", sep = "")
