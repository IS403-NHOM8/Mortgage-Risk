library(lubridate)
library(gpboost)
library(stringr)

source("R/functions_data.R")

set.seed(1)

pred_years <- 2017:2022
out_dir <- file.path("3_training_and_prediction", "GPBoost_spatial", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (pred_year in pred_years) {
  val_year <- pred_year - 1

  pred_file_year <- file.path(out_dir, paste0("prediction_", pred_year, ".rds"))
  param_file_year <- file.path(out_dir, paste0("parameters_", val_year, ".rds"))

  if (file.exists(pred_file_year) && file.exists(param_file_year)) next

  hyper_candidates <- c(
    file.path("2_hyperparameter_tuning", "GPBoost_spatial", paste0("data_", val_year), paste0("hyperpars_", val_year, ".rds")),
    file.path("2_hyperparameter_tuning", "GPBoost_spatial", paste0("hyperpars_", val_year, ".rds"))
  )
  hyper_file <- hyper_candidates[file.exists(hyper_candidates)][1]

  if (is.na(hyper_file) || !file.exists(hyper_file)) {
    stop(sprintf("Missing hyperparameters file for validation year %d.", val_year))
  }

  opt_hyperpars <- readRDS(hyper_file)

  data_path <- file.path("1_data_set", "data", "processed", "freddie_mac", paste0("data_window_", pred_year, ".rds"))
  vers_data <- readRDS(data_path)

  X <- vers_data$X
  Y <- vers_data$Y

  years <- as.numeric(format(X$date_versioning, "%Y"))
  i_test <- (years == pred_year)
  i_train <- (!i_test)

  imputed_obj <- impute_missing_leakage_safe(X, train_mask = i_train, test_mask = i_test)
  X <- imputed_obj$data

  exclude_cols <- c("current_upb", "date_versioning")
  X_features <- X[, !(names(X) %in% exclude_cols)]
  X_design <- model.matrix(~ ., data = X_features)

  if (nrow(X) != nrow(X_design)) {
    stop("Row count mismatch between X and X_design.")
  }

  X_train <- X_design[i_train, ]
  coords_train <- X[i_train, c("X_centroid", "Y_centroid")]
  Y_train <- Y[i_train]
  gpboost_dataset <- gpb.Dataset(data = X_train[, -1], label = Y_train)

  X_test <- X_design[i_test, ]
  coords_test <- X[i_test, c("X_centroid", "Y_centroid")]
  Y_test <- Y[i_test]

  gp_model <- GPModel(
    gp_coords = coords_train,
    cov_function = "matern",
    cov_fct_shape = 1.5,
    likelihood = "bernoulli_logit",
    gp_approx = "vecchia",
    vecchia_ordering = "random",
    num_neighbors = 20L,
    matrix_inversion_method = "cholesky"
  )

  gp_model$set_prediction_data(
    vecchia_pred_type = "latent_order_obs_first_cond_obs_only",
    num_neighbors_pred = 20L
  )

  time_start <- Sys.time()
  gpboost_model <- gpb.train(
    data = gpboost_dataset,
    gp_model = gp_model,
    line_search_step_length = FALSE,
    num_leaves = 2^10,
    nrounds = opt_hyperpars$best_iter,
    learning_rate = opt_hyperpars$learning_rate,
    max_depth = opt_hyperpars$max_depth,
    min_data_in_leaf = opt_hyperpars$min_data_in_leaf,
    lambda_l2 = opt_hyperpars$lambda_l2,
    verbose = 0
  )
  train_time_secs <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

  pred_response <- predict(
    gpboost_model,
    data = X_test[, -1],
    gp_coords_pred = as.matrix(coords_test),
    predict_var = FALSE,
    predict_cov_mat = FALSE,
    pred_latent = FALSE
  )

  pred_latent <- predict(
    gpboost_model,
    data = X_test[, -1],
    gp_coords_pred = as.matrix(coords_test),
    predict_var = FALSE,
    predict_cov_mat = FALSE,
    pred_latent = TRUE
  )

  year_pred_obj <- list(
    pred_response_mean = pred_response$response_mean,
    pred_latent_FE = pred_latent$fixed_effect,
    real_defaults = Y_test,
    current_upb = X$current_upb[i_test],
    coords_test = coords_test
  )

  year_param_obj <- data.frame(
    pred_year = pred_year,
    val_year = val_year,
    model = "GPBoost_spatial",
    GP_var = gp_model$get_cov_pars()[1],
    GP_range_space = gp_model$get_cov_pars()[2],
    final_neg_log_likelihood = gp_model$get_current_neg_log_likelihood(),
    time_training_mins = train_time_secs / 60
  )

  year_pred_wrapper <- list()
  year_pred_wrapper[[paste0("pred_year_", pred_year)]] <- year_pred_obj
  saveRDS(year_pred_wrapper, pred_file_year)
  saveRDS(year_param_obj, param_file_year)

  rm(vers_data, X, Y, X_design, X_train, X_test, gpboost_dataset, gp_model, gpboost_model)
  gc()
}
