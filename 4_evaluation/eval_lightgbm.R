library(data.table)
library(pROC)
library(PRROC)

# 1. Nạp kết quả dự đoán năm 2016
pred_df <- readRDS("3_training_and_prediction/LightGBM/data/predictions_2016.rds")
y_true <- pred_df$Y_true
y_prob <- pred_df$pred_prob

# Cắt ngưỡng cực biên để tránh log(0) khi tính Log-Loss
eps <- 1e-15
y_prob_clip <- pmin(pmax(y_prob, eps), 1 - eps)

# 2. Tính 5 độ đo đánh giá xác suất vỡ nợ
# Độ đo 1: Log-Loss (Negative Log-Likelihood - càng nhỏ càng tốt)
log_loss <- -mean(y_true * log(y_prob_clip) + (1 - y_true) * log(1 - y_prob_clip))

# Độ đo 2: Brier Score (càng nhỏ càng tốt)
brier_score <- mean((y_true - y_prob)^2)

# Độ đo 3: AUC-ROC (càng gần 1 càng tốt)
roc_obj <- roc(response = y_true, predictor = y_prob, quiet = TRUE)
auc_roc <- as.numeric(auc(roc_obj))

# Độ đo 4: AUC-PR (Area Under Precision-Recall Curve - càng gần 1 càng tốt)
pr_obj <- pr.curve(scores.class0 = y_prob[y_true == 1], scores.class1 = y_prob[y_true == 0], curve = FALSE)
auc_pr <- pr_obj$auc.integral

# Độ đo 5: Tjur's R2 / Discrimination Slope (Chênh lệch xác suất trung bình giữa nhóm vỡ nợ và không vỡ nợ)
tjur_r2 <- mean(y_prob[y_true == 1]) - mean(y_prob[y_true == 0])

# 3. Tổng hợp và lưu bảng kết quả
eval_summary <- data.table(
  Model = "LightGBM",
  Test_Year = 2016,
  Log_Loss = round(log_loss, 6),
  Brier_Score = round(brier_score, 6),
  AUC_ROC = round(auc_roc, 6),
  AUC_PR = round(auc_pr, 6),
  Tjur_R2 = round(tjur_r2, 6)
)

print(eval_summary)
fwrite(eval_summary, "results/LightGBM/evaluation_metrics_2016.csv")
