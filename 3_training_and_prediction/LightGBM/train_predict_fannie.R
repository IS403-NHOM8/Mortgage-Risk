library(lightgbm)
library(data.table)

data_dir <- "1_data_set/data/processed/fannie_mae"
best_params_list <- readRDS("results/LightGBM_Fannie_Paper/tuning/all_best_params_paper.rds")
pred_dir <- "3_training_and_prediction/LightGBM/data_fannie_paper"
dir.create(pred_dir, recursive = TRUE, showWarnings = FALSE)

test_years <- 2017:2022
all_predictions <- list()

for (y_test in test_years) {
  file_path <- file.path(data_dir, sprintf("data_window_%d.rds", y_test))
  val_key   <- as.character(y_test - 1)
  
  cat(sprintf("=== Final Train (2011-%d) -> Dự báo Test (%d) | Tham số Val %s ===\n", y_test - 1, y_test, val_key))
  dat <- readRDS(file_path)
  
  X_all  <- as.data.frame(dat$X)
  Y_all  <- dat$Y
  ID_all <- dat$ID
  dates  <- X_all$date_versioning
  years  <- as.integer(format(dates, "%Y"))
  
  cat_cols <- names(X_all)[sapply(X_all, is.factor)]
  X_all$date_versioning <- NULL
  for (col in cat_cols) {
    X_all[[col]] <- as.integer(X_all[[col]]) - 1L
  }
  X_mat <- as.matrix(X_all)
  
  train_idx <- which(years >= 2011 & years <= (y_test - 1))
  test_idx  <- which(years == y_test)
  
  dtrain <- lgb.Dataset(data = X_mat[train_idx, ], label = Y_all[train_idx], categorical_feature = cat_cols)
  
  params <- best_params_list[[val_key]]
  nrounds_opt <- params$nrounds
  params$nrounds <- NULL
  params$val_year <- NULL
  params$val_auc <- NULL
  
  set.seed(42)
  model <- lgb.train(params = params, data = dtrain, nrounds = nrounds_opt, verbose = -1)
  preds <- predict(model, X_mat[test_idx, ])
  
  year_pred_dt <- data.table(
    ID              = ID_all[test_idx],
    date_versioning = dates[test_idx],
    test_year       = y_test,
    Y_true          = Y_all[test_idx],
    pred_prob       = preds
  )
  
  saveRDS(year_pred_dt, file.path(pred_dir, sprintf("pred_%d.rds", y_test)))
  all_predictions[[as.character(y_test)]] <- year_pred_dt
  rm(dat, X_all, X_mat, dtrain, model); gc()
}

combined_preds <- rbindlist(all_predictions)
saveRDS(combined_preds, file.path(pred_dir, "combined_predictions_paper.rds"))
cat(sprintf("\nHoàn tất! Đã dự báo %d khoản vay giai đoạn 2017-2022.\n", nrow(combined_preds)))
