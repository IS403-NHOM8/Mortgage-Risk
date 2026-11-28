
# ============================================================
# combine_years.R
# Combine kết quả GPBoost Spatio-Temporal cho 6 năm 2017-2022
# ============================================================

# -------- ĐƯỜNG DẪN --------
base_dir <- paste0(
  "D:/UIT_DOCUMENTS/NAM_3/",
  "PHÂN TÍCH DỮ LIỆU KINH DOANH-20260704T155950Z-3-001/",
  "Đồ án/Mortgage-Risk"
)

out_dir <- file.path(
  base_dir,
  "3_training_and_prediction",
  "GPBoost_spatio_temporal",
  "data"
)

if (!dir.exists(out_dir)) {
  stop("Không tìm thấy thư mục output: ", out_dir)
}

# -------- CÁC NĂM DỰ BÁO --------
pred_years <- 2017:2022
n_pred_years <- length(pred_years)

cat("============================================================\n")
cat("COMBINE GPBOOST SPATIO-TEMPORAL RESULTS\n")
cat("Prediction years:", min(pred_years), "-", max(pred_years), "\n")
cat("Number of years:", n_pred_years, "\n")
cat("Output directory:", out_dir, "\n")
cat("============================================================\n")

# -------- TÊN CÁC CỘT PARAMETERS --------
par_cnames <- c(
  "year",
  "model",
  "GP_var",
  "GP_range_time",
  "GP_range_space",
  "INIT_GP_var",
  "INIT_GP_range_time",
  "INIT_GP_range_space",
  "final_neg_log_likelihood",
  "time_training"
)

# -------- KIỂM TRA VÀ ĐỌC KẾT QUẢ TỪNG NĂM --------
parameters_list <- vector("list", n_pred_years)

predictions <- vector("list", n_pred_years)
names(predictions) <- paste0("pred_year_", pred_years)

# Lưu thêm thống kê imputation của từng năm
imputation_stats <- vector("list", n_pred_years)
names(imputation_stats) <- paste0("pred_year_", pred_years)

for (i in seq_along(pred_years)) {
  
  pred_year <- pred_years[i]
  val_year  <- pred_year - 1
  
  cat("\n--------------------------------------------\n")
  cat("Processing prediction year:", pred_year, "\n")
  cat("Validation year:", val_year, "\n")
  
  # Đường dẫn file
  par_path <- file.path(
    out_dir,
    paste0("parameters_", val_year, ".rds")
  )
  
  pred_path <- file.path(
    out_dir,
    paste0("prediction_", pred_year, ".rds")
  )
  
  imp_path <- file.path(
    out_dir,
    paste0("imputation_stats_", pred_year, ".rds")
  )
  
  # Kiểm tra file bắt buộc
  required_files <- c(par_path, pred_path, imp_path)
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Thiếu file kết quả cho năm ", pred_year, ":\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  # Đọc parameters
  y_parameters <- readRDS(par_path)
  
  if (!is.data.frame(y_parameters) || nrow(y_parameters) != 1) {
    stop("File parameters không đúng cấu trúc: ", par_path)
  }
  
  if (!all(par_cnames %in% names(y_parameters))) {
    stop("Thiếu cột trong file parameters: ", par_path)
  }
  
  y_parameters <- y_parameters[, par_cnames, drop = FALSE]
  
  # Kiểm tra năm trong parameters phải là validation year
  if (as.integer(y_parameters$year[1]) != val_year) {
    stop(
      "Năm trong parameters không khớp: ",
      par_path
    )
  }
  
  parameters_list[[i]] <- y_parameters
  
  # Đọc prediction
  y_prediction <- readRDS(pred_path)
  
  expected_name <- paste0("pred_year_", pred_year)
  
  if (!is.list(y_prediction) ||
      is.null(y_prediction[[expected_name]])) {
    stop(
      "Cấu trúc prediction không đúng cho năm ",
      pred_year
    )
  }
  
  # Lấy nội dung prediction của năm đó
  predictions[[i]] <- y_prediction[[expected_name]]
  
  # Đọc imputation statistics
  imputation_stats[[i]] <- readRDS(imp_path)
  
  cat("Parameters: OK\n")
  cat("Prediction: OK\n")
  cat("Imputation stats: OK\n")
}

# -------- GỘP PARAMETERS --------
parameters <- do.call(rbind, parameters_list)
rownames(parameters) <- NULL

if (nrow(parameters) != n_pred_years) {
  stop("Số dòng parameters sau khi gộp không bằng số năm dự báo.")
}

# -------- KIỂM TRA PREDICTIONS --------
if (length(predictions) != n_pred_years) {
  stop("Số lượng prediction sau khi gộp không đúng.")
}

# -------- LƯU HAI FILE TỔNG HỢP THEO BÀI BÁO --------
out_parameters <- file.path(
  out_dir,
  "parameters_GPBoost_spatio_temporal.rds"
)

out_predictions <- file.path(
  out_dir,
  "prediction_GPBoost_spatio_temporal.rds"
)

saveRDS(parameters, out_parameters)
saveRDS(predictions, out_predictions)

# -------- LƯU THÊM IMPUTATION STATS CỦA NHÓM --------
out_imputation <- file.path(
  out_dir,
  "imputation_stats_GPBoost_spatio_temporal.rds"
)

saveRDS(imputation_stats, out_imputation)

# -------- THÔNG BÁO KẾT QUẢ --------
cat("\n============================================================\n")
cat("COMBINE HOÀN TẤT!\n")
cat("============================================================\n")

cat("\nParameters:\n", out_parameters, "\n")
cat("Số dòng:", nrow(parameters), "\n")

cat("\nPredictions:\n", out_predictions, "\n")
cat("Số năm:", length(predictions), "\n")

cat("\nImputation statistics:\n", out_imputation, "\n")
cat("Số năm:", length(imputation_stats), "\n")

cat("\nCác năm đã tổng hợp:\n")
print(names(predictions))

cat("\nXONG!\n")
