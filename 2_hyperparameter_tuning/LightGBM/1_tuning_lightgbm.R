library(lightgbm)
library(data.table)

# 1. Đọc dữ liệu window 2015 (chứa 2011 - 2015)
data_2015 <- readRDS("1_data_set/data/processed/freddie_mac/data_window_2015.rds")

X_all <- data_2015$X
Y_all <- data_2015$Y

# Tách năm từ cột date_versioning
years <- as.integer(format(X_all$date_versioning, "%Y"))

# Xác định các cột phân loại (Factor) để khai báo cho LightGBM
cat_cols <- names(X_all)[sapply(X_all, is.factor)]

# Chuẩn bị ma trận đặc trưng: loại bỏ cột date_versioning và chuyển Factor sang số nguyên (bắt đầu từ 0)
X_prep <- as.data.frame(X_all)
X_prep$date_versioning <- NULL
for (col in cat_cols) {
  X_prep[[col]] <- as.integer(X_prep[[col]]) - 1L
}
X_mat <- as.matrix(X_prep)

# 2. Chia tập Train (2011-2014) và Validation (2015)
train_idx <- which(years %in% 2011:2014)
val_idx   <- which(years == 2015)

dtrain <- lgb.Dataset(
  data = X_mat[train_idx, ],
  label = Y_all[train_idx],
  categorical_feature = cat_cols
)

dval <- lgb.Dataset.create.valid(
  dataset = dtrain,
  data = X_mat[val_idx, ],
  label = Y_all[val_idx]
)

# 3. Tạo lưới siêu tham số (Grid Search)
param_grid <- expand.grid(
  learning_rate = c(0.01, 0.05, 0.1),
  num_leaves = c(15, 31, 63),
  min_data_in_leaf = c(50, 200),
  feature_fraction = c(0.8, 1.0),
  stringsAsFactors = FALSE
)

results_list <- list()
best_logloss <- Inf
best_params <- NULL

cat("Bắt đầu chạy Tuning với", nrow(param_grid), "tổ hợp tham số...\n")

for (i in seq_len(nrow(param_grid))) {
  set.seed(42)
  
  params <- list(
    objective = "binary",
    metric = "binary_logloss",
    boosting = "gbdt",
    learning_rate = param_grid$learning_rate[i],
    num_leaves = param_grid$num_leaves[i],
    min_data_in_leaf = param_grid$min_data_in_leaf[i],
    feature_fraction = param_grid$feature_fraction[i],
    feature_pre_filter = FALSE,
    num_threads = 4,
    verbose = -1
  )
  
  model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 2000,
    valids = list(valid = dval),
    early_stopping_rounds = 50,
    verbose = -1
  )
  
  val_loss <- model$best_score
  best_iter <- model$best_iter
  
  results_list[[i]] <- cbind(param_grid[i, ], best_iter = best_iter, val_logloss = val_loss)
  cat(sprintf("[%d/%d] LogLoss: %.6f | Iter: %d\n", i, nrow(param_grid), val_loss, best_iter))
  
  if (val_loss < best_logloss) {
    best_logloss <- val_loss
    best_params <- c(params, list(nrounds = best_iter))
  }
}

# 4. Lưu kết quả Tuning
tuning_df <- rbindlist(results_list)
tuning_df <- tuning_df[order(val_logloss)]

dir.create("results/LightGBM", recursive = TRUE, showWarnings = FALSE)
fwrite(tuning_df, "results/LightGBM/tuning_results.csv")
saveRDS(best_params, "results/LightGBM/best_params.rds")

cat("Hoàn tất Tuning! Bộ tham số tốt nhất đã được lưu vào results/LightGBM/best_params.rds\n")
print(tuning_df[1, ])
