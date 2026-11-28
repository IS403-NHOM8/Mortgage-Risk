library(lubridate)
library(gpboost)

set.seed(1)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript train_predict_parallel.R <YYYY>")
pred_year <- as.integer(args[1])
if (is.na(pred_year)) stop("Invalid year: ", args[1])
val_year <- pred_year - 1

data_path      <- file.path("1_data_set", "data", "processed", "freddie_mac", "data_window_2022.rds")
hyperpars_path <- file.path("2_hyperparameter_tuning", "GPBoost_spatio_temporal",
                            paste0("data_", val_year), paste0("hyperpars_", val_year, ".rds"))
out_dir        <- file.path("3_training_and_prediction", "GPBoost_spatio_temporal", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(hyperpars_path)) stop("Missing hyperpars: ", hyperpars_path)
if (!file.exists(data_path))      stop("Missing data: ", data_path)

cat("Prediction year:", pred_year, "| Validation year:", val_year, "\n")

predictions <- vector("list", 1)
names(predictions) <- paste0("pred_year_", pred_year)

par_cnames <- c("year", "model",
                "GP_var", "GP_range_time", "GP_range_space",
                "INIT_GP_var", "INIT_GP_range_time", "INIT_GP_range_space",
                "final_neg_log_likelihood", "time_training")
parameters <- as.data.frame(matrix(nrow = 1, ncol = length(par_cnames)))
colnames(parameters) <- par_cnames

opt_hyperpars <- readRDS(hyperpars_path)
vers_data     <- readRDS(data_path)
X_all <- vers_data$X
Y_all <- vers_data$Y
if (nrow(X_all) != length(Y_all)) stop("X and Y length mismatch")

year_all <- year(X_all$date_versioning)
if (anyNA(year_all)) stop("date_versioning has NA")

keep_idx <- year_all <= pred_year
if (!any(keep_idx)) stop("No data <= ", pred_year)

X        <- X_all[keep_idx, , drop = FALSE]
Y        <- Y_all[keep_idx]
year_vec <- year_all[keep_idx]

i_train <- year_vec < pred_year
i_test  <- year_vec == pred_year
if (!any(i_train)) stop("No training data before ", pred_year)
if (!any(i_test))  stop("No test data for ", pred_year)

exclude_vars  <- c("current_upb", "date_versioning")
feature_names <- setdiff(names(X), exclude_vars)
X_features    <- X[, feature_names, drop = FALSE]

categorical_vars <- intersect(
  c("occupancy", "nr_units", "loan_purpose",
    "first_time_homebuyer", "MSA", "number_of_borrowers"),
  feature_names
)

factor_levels <- list(
  occupancy            = c("P", "I", "S"),
  nr_units             = c("1", "2", "3", "4"),
  loan_purpose         = c("P", "N", "C"),
  first_time_homebuyer = c("0", "1"),
  MSA                  = c("0", "1"),
  number_of_borrowers  = c("1", "2")
)

get_mode <- function(x) {
  x <- as.character(x); x <- x[!is.na(x)]
  if (!length(x)) stop("Cannot compute mode: all NA in train")
  names(which.max(table(x)))
}

imputation_stats <- list(numeric_median = list(), categorical_mode = list())

for (v in feature_names) {
  if (v %in% categorical_vars) {
    x_char  <- as.character(X_features[[v]])
    allowed <- factor_levels[[v]]
    unknown <- setdiff(unique(x_char[!is.na(x_char)]), allowed)
    if (length(unknown)) stop("Unknown levels in ", v, ": ", paste(unknown, collapse = ", "))
    m <- get_mode(x_char[i_train])
    imputation_stats$categorical_mode[[v]] <- m
    x_char[is.na(x_char)] <- m
    X_features[[v]] <- factor(x_char, levels = allowed)
  } else {
    if (!is.numeric(X_features[[v]]))
      stop("Column ", v, " is not numeric and not declared categorical")
    med <- median(X_features[[v]][i_train], na.rm = TRUE)
    if (!is.finite(med)) stop("Cannot compute median for ", v)
    imputation_stats$numeric_median[[v]] <- med
    X_features[[v]][is.na(X_features[[v]])] <- med
  }
}
if (anyNA(X_features)) stop("NA remains after imputation")

X_design <- model.matrix(~ ., data = X_features)
if (nrow(X_design) != nrow(X)) stop("X and X_design row mismatch")
if (anyNA(X_design) || any(!is.finite(X_design))) stop("X_design has NA/Inf")

X_train <- X_design[i_train, , drop = FALSE]
X_test  <- X_design[i_test,  , drop = FALSE]

coords_train <- cbind(
  time = year_vec[i_train],
  X[i_train, c("X_centroid", "Y_centroid"), drop = FALSE]
)
coords_test <- cbind(
  time = year_vec[i_test],
  X[i_test, c("X_centroid", "Y_centroid"), drop = FALSE]
)

Y_train <- Y[i_train]
Y_test  <- Y[i_test]

if (nrow(X_train) != length(Y_train)) stop("X_train/Y_train mismatch")
if (nrow(X_test)  != length(Y_test))  stop("X_test/Y_test mismatch")
if (!identical(colnames(X_train), colnames(X_test))) stop("Column names mismatch between train and test")

gpboost_dataset <- gpb.Dataset(data = X_train[, -1, drop = FALSE], label = Y_train)

cat("Train:", nrow(X_train), "pts | Test:", nrow(X_test), "pts | FE:", ncol(X_train) - 1, "\n")

gp_model <- GPModel(
  gp_coords = coords_train,
  cov_function = "matern_space_time",
  cov_fct_shape = 1.5,
  likelihood = "bernoulli_logit",
  gp_approx = "vecchia",
  vecchia_ordering = "time_random_space",
  num_neighbors = 20L,
  matrix_inversion_method = "iterative"
)
gp_model$set_prediction_data(
  vecchia_pred_type  = "latent_order_obs_first_cond_obs_only",
  num_neighbors_pred = 20L
)

if (val_year == 2007) {
  gp_model$set_optim_params(list(trace = TRUE, init_cov_pars = c(0.8, 2, 9), seed_rand_vec_trace = 1))
} else {
  gp_model$set_optim_params(list(trace = TRUE, seed_rand_vec_trace = 1))
}

parameters$time_training[1] <- system.time(
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
    verbose = 2
  )
)[3]

parameters$year[1]   <- val_year
parameters$model[1]  <- "GPBoost_spatio_temporal"
cov_pars <- gp_model$get_cov_pars()
opt_pars <- gp_model$get_optim_params()$init_cov_pars
parameters$GP_var[1]         <- cov_pars[1]
parameters$GP_range_time[1]  <- cov_pars[2]
parameters$GP_range_space[1] <- cov_pars[3]
parameters$INIT_GP_var[1]         <- opt_pars[1]
parameters$INIT_GP_range_time[1]  <- opt_pars[2]
parameters$INIT_GP_range_space[1] <- opt_pars[3]
parameters$final_neg_log_likelihood[1] <- gp_model$get_current_neg_log_likelihood()

i_unique <- which(!duplicated(coords_train))
train_coords_unique <- coords_train[i_unique, , drop = FALSE]
X_train_unique      <- X_train[i_unique, , drop = FALSE]

train_latent <- predict(
  gpboost_model,
  data = X_train_unique[, -1, drop = FALSE],
  gp_coords_pred = as.matrix(train_coords_unique),
  predict_var = FALSE, predict_cov_mat = FALSE, pred_latent = TRUE
)

pred_response <- predict(
  gpboost_model,
  data = X_test[, -1, drop = FALSE],
  gp_coords_pred = as.matrix(coords_test),
  predict_var = FALSE, predict_cov_mat = FALSE, pred_latent = FALSE
)
pred_latent <- predict(
  gpboost_model,
  data = X_test[, -1, drop = FALSE],
  gp_coords_pred = as.matrix(coords_test),
  predict_var = FALSE, predict_cov_mat = FALSE, pred_latent = TRUE
)

i_unique <- which(!duplicated(coords_test))
coords_test_unique <- coords_test[i_unique, , drop = FALSE]
X_test_unique      <- X_test[i_unique, , drop = FALSE]

b_to_obs <- rep(NA, nrow(coords_test))
for (i in seq_len(nrow(coords_test))) {
  b_to_obs[i] <- which(
    coords_test[i, "X_centroid"] == coords_test_unique[, "X_centroid"] &
    coords_test[i, "Y_centroid"] == coords_test_unique[, "Y_centroid"] &
    coords_test[i, "time"]       == coords_test_unique[, "time"]
  )
}
if (any(is.na(b_to_obs))) stop("NA in b_to_obs")
if (!all(coords_test_unique[b_to_obs, ] == coords_test)) stop("latent match failed")

pred_latent_unique <- predict(
  gpboost_model,
  data = X_test_unique[, -1, drop = FALSE],
  gp_coords_pred = as.matrix(coords_test_unique),
  predict_var = FALSE, predict_cov_mat = TRUE, pred_latent = TRUE
)

predictions[[1]] <- list(
  pred_response_mean  = pred_response$response_mean,
  pred_latent_FE      = pred_latent$fixed_effect,
  pred_latent_mean_b  = pred_latent_unique$random_effect_mean,
  pred_latent_cov     = pred_latent_unique$random_effect_cov,
  pred_latent_coords  = coords_test_unique,
  b_to_obs            = b_to_obs,
  train_latent_mean_b = train_latent$random_effect_mean,
  train_coords_unique = train_coords_unique
)

saveRDS(predictions,      file.path(out_dir, paste0("prediction_", pred_year, ".rds")))
saveRDS(parameters,       file.path(out_dir, paste0("parameters_", val_year, ".rds")))
saveRDS(imputation_stats, file.path(out_dir, paste0("imputation_stats_", pred_year, ".rds")))

cat("Saved to", out_dir, "\n")
