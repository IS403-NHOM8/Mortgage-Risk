library(lightgbm)
library(data.table)
library(pROC)

data_dir <- "1_data_set/data/processed/fannie_mae"
out_dir  <- "results/LightGBM_Fannie_Paper/tuning"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

param_grid <- expand.grid(
  learning_rate    = c(0.1, 0.05, 0.01),
  max_depth        = c(2, 3, 5, 10),
  min_data_in_leaf = c(10, 100, 1000),
  lambda_l2        = c(0, 1, 10),
  stringsAsFactors = FALSE
)

val_years <- 2016:2021
all_best_params <- list()

for (y_val in val_years) {
  file_path <- file.path(data_dir, sprintf("data_window_%d.rds", y_val))
  if (!file.exists(file_path)) stop(paste("Không tìm thấy file:", file_path))
  
  cat(sprintf("\n=== Tuning theo Paper: Inner Train (2011-%d) | Validation (%d) ===\n", y_val - 1, y_val))
  dat <- readRDS(file_path)
  
  X_all <- as.data.frame(dat$X)
  Y_all <- dat$Y
  years <- as.integer(format(X_all$date_versioning, "%Y"))
  
  cat_cols <- names(X_all)[sapply(X_all, is.factor)]
  X_all$date_versioning <- NULL
  for (col in cat_cols) {
    X_all[[col]] <- as.integer(X_all[[col]]) - 1L
  }
  X_mat <- as.matrix(X_all)
  
  train_idx <- which(years >= 2011 & years <= (y_val - 1))
  val_idx   <- which(years == y_val)
  
  dtrain <- lgb.Dataset(data = X_mat[train_idx, ], label = Y_all[train_idx], categorical_feature = cat_cols)
  dval   <- lgb.Dataset.create.valid(dataset = dtrain, data = X_mat[val_idx, ], label = Y_all[val_idx])
  X_val_mat <- X_mat[val_idx, ]
  Y_val_vec <- Y_all[val_idx]
  
  best_auc <- -Inf
  best_row <- NULL
  year_results <- list()
  
  for (i in seq_len(nrow(param_grid))) {
    set.seed(42)
    params <- list(
      objective          = "binary",
      metric             = "auc",
      boosting           = "gbdt",
      learning_rate      = param_grid$learning_rate[i],
      max_depth          = param_grid$max_depth[i],
      num_leaves         = 1024, # Mở khóa để max_depth kiểm soát độ sâu giống bài báo
      min_data_in_leaf   = param_grid$min_data_in_leaf[i],
      lambda_l2          = param_grid$lambda_l2[i],
      feature_pre_filter = FALSE,
      num_threads        = max(1, parallel::detectCores() - 1),
      verbose            = -1
    )

    model <- lgb.train(
      params = params, data = dtrain, nrounds = 1000,
      valids = list(valid = dval), early_stopping_rounds = 50, verbose = -1
    )
    
    val_auc   <- model$best_score
    best_iter <- model$best_iter
    year_results[[i]] <- cbind(val_year = y_val, param_grid[i, ], best_iter = best_iter, val_auc = val_auc)
    
    if (val_auc > best_auc) {
      best_auc <- val_auc
      best_row <- c(params, list(nrounds = best_iter, val_year = y_val, val_auc = val_auc))
    }
  }
  
  res_dt <- rbindlist(year_results)[order(-val_auc)] 
  fwrite(res_dt, file.path(out_dir, sprintf("tuning_val_%d.csv", y_val)))
  all_best_params[[as.character(y_val)]] <- best_row
  
  cat(sprintf("-> Xong Val %d | Max Val AUC: %.6f | Iter: %d\n", y_val, best_auc, best_row$nrounds))
  rm(dat, X_all, X_mat, X_val_mat, dtrain, dval); gc()
}

saveRDS(all_best_params, file.path(out_dir, "all_best_params_paper.rds"))
cat("\nHoàn tất Tuning!\n")
