library(lubridate)
library(gpboost)
library(stringr)

source("R/functions_data.R")
source("2_hyperparameter_tuning/gpb.cv.R")

set.seed(1)

val_years <- 2016:2021

full_param_grid <- list(
  "learning_rate" = c(10, 1, 0.1),
  "min_data_in_leaf" = c(10, 100, 1000),
  "max_depth" = c(2, 3, 5, 10),
  "lambda_l2" = c(0, 1, 10)
)
n_combs <- get.grid.size(full_param_grid)
other_params <- list(num_leaves = 2^10, verbose = -1)

for (val_year in val_years) {
  out_dir_year <- file.path("2_hyperparameter_tuning", "GPBoost_spatial", paste0("data_", val_year))
  dir.create(out_dir_year, recursive = TRUE, showWarnings = FALSE)

  data_path <- file.path("1_data_set", "data", "processed", "freddie_mac", paste0("data_window_", val_year, ".rds"))
  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }
  vers_data <- readRDS(data_path)

  X <- vers_data$X
  Y <- vers_data$Y

  years <- as.numeric(format(X$date_versioning, "%Y"))
  i_val <- (years == val_year)
  i_train <- (years < val_year)

  imputed_obj <- impute_missing_leakage_safe(X, train_mask = i_train, test_mask = i_val)
  X <- imputed_obj$data

  exclude_cols <- c("current_upb", "date_versioning")
  X_features <- X[, !(names(X) %in% exclude_cols)]
  X_design <- model.matrix(~ ., data = X_features)

  if (nrow(X) != nrow(X_design)) {
    stop("Row count mismatch between X and X_design.")
  }

  coords <- X[, c("X_centroid", "Y_centroid")]
  boost_dataset <- gpb.Dataset(data = X_design[, -1], label = Y)
  folds <- list(which(i_val))

  for (i_param in 0:(n_combs - 1)) {
    out_file <- file.path(out_dir_year, paste0("param_comb_", i_param, ".rds"))
    if (file.exists(out_file)) next

    param_grid <- get.param.combination(i_param, full_param_grid)

    gp_model <- GPModel(
      gp_coords = coords,
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

    time_comb_start <- Sys.time()
    tune_res <- tryCatch({
      gpb.grid.search.tune.parameters(
        param_grid = param_grid,
        params = other_params,
        line_search_step_length = FALSE,
        num_try_random = NULL,
        train_gp_model_cov_pars = TRUE,
        folds = folds,
        data = boost_dataset,
        verbose_eval = 0,
        gp_model = gp_model,
        nrounds = 1000,
        early_stopping_rounds = 20,
        eval = "auc"
      )
    }, error = function(e) {
      warning(sprintf("Error at combination %d, year %d: %s", i_param, val_year, e$message))
      return(NULL)
    })
    comb_secs <- as.numeric(difftime(Sys.time(), time_comb_start, units = "secs"))

    if (!is.null(tune_res)) {
      param_comb <- data.frame(
        year = val_year,
        model = "GPBoost_spatial",
        learning_rate = tune_res$best_params$learning_rate,
        min_data_in_leaf = tune_res$best_params$min_data_in_leaf,
        max_depth = tune_res$best_params$max_depth,
        lambda_l2 = tune_res$best_params$lambda_l2,
        best_iter = tune_res$best_iter,
        best_score = tune_res$best_score,
        time_grid_search = comb_secs
      )
      saveRDS(param_comb, out_file)
    }

    rm(gp_model, tune_res)
    if (i_param %% 10 == 0) gc()
  }

  rm(vers_data, X, Y, X_design, X_features, boost_dataset, coords)
  gc()
}
