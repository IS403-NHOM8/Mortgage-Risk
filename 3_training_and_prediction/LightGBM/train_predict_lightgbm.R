library(lightgbm)
library(data.table)

# 1. Đọc dữ liệu window 2016 (chứa 2011 - 2016) và bộ tham số tốt nhất
data_2016 <- readRDS("1_data_set/data/processed/freddie_mac/data_window_2016.rds")
best_params <- readRDS("results/LightGBM/best_params.rds")

X_all <- data_2016$X
Y_all <- data_2016$Y
ID_all <- data_2016$ID

years <- as.integer(format(X_all$date_versioning, "%Y"))
cat_cols <- names(X_all)[sapply(X_all, is.factor)]

# Tiền xử lý giống hệt bước Tuning
X_prep <- as.data.frame(X_all)
X_prep$date_versioning <- NULL
for (col in cat_cols) {
  X_prep[[col]] <- as.integer(X_prep[[col]]) - 1L
}
X_mat <- as.matrix(X_prep)

# 2. Chia tập Train (2011-2015) và Test (2016)
# Lưu ý: Nếu nhóm trưởng quy định chỉ train trên 2011-2014 thì sửa 2011:2015 thành 2011:2014
train_idx <- which(years %in% 2011:2015)
test_idx  <- which(years == 2016)

dtrain <- lgb.Dataset(
  data = X_mat[train_idx, ],
  label = Y_all[train_idx],
  categorical_feature = cat_cols
)

# 3. Huấn luyện mô hình cuối cùng
set.seed(42)
nrounds_opt <- best_params$nrounds
best_params$nrounds <- NULL

final_model <- lgb.train(
  params = best_params,
  data = dtrain,
  nrounds = nrounds_opt,
  verbose = -1
)

# 4. Dự báo xác suất vỡ nợ trên tập Test 2016
pred_probs <- predict(final_model, X_mat[test_idx, ])

# 5. Lưu kết quả dự báo (gồm ID, nhãn thực tế Y_test và xác suất dự báo pred_prob)
pred_output <- data.table(
  ID = ID_all[test_idx],
  date_versioning = X_all$date_versioning[test_idx],
  Y_true = Y_all[test_idx],
  pred_prob = pred_probs
)

dir.create("3_training_and_prediction/LightGBM/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(pred_output, "3_training_and_prediction/LightGBM/data/predictions_2016.rds")

cat("Hoàn tất Training & Prediction! Đã dự báo cho", nrow(pred_output), "khoản vay năm 2016.\n")
