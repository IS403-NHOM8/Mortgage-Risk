library(LiblineaR)
stopifnot(requireNamespace("pROC", quietly = TRUE))

# 1. Cau hinh va duong dan
VERSION <- "svc_weighted_rff"
SCOPE <- "retrospective_exploratory"
COSTS <- c(.001, .003, .01, .03, .1)
GAMMAS <- c(1e-4, 3e-4, .001, .003, .01)
root <- "3_training_and_prediction/SVC/data"
tuning <- "2_hyperparameter_tuning/SVC"
data_dir <- "1_data_set/data/processed/freddie_mac"
years <- 2017:2022
CATS <- list(occupancy = c("P", "S", "I"), nr_units = as.character(1:4),
             loan_purpose = c("C", "N", "P"), first_time_homebuyer = c("0", "1"),
             MSA = c("0", "1"), number_of_borrowers = c("1", "2"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
input <- function(y) file.path(data_dir, sprintf("data_window_%d.rds", y))

# 2. Doc va kiem tra sieu tham so tot nhat hang nam
read_best <- function(y) {
  v <- y - 1L
  r <- readRDS(file.path(tuning, paste0("data_", v), sprintf("best_param_comb_%d.rds", v)))
  p <- r$params
  stopifnot(identical(r$version, VERSION), identical(r$evaluation_scope, SCOPE),
            r$val_year == v, r$test_year == y,
            identical(r$data_md5, unname(tools::md5sum(input(v)))),
            identical(r$package_version, as.character(packageVersion("LiblineaR"))),
            identical(r$solver, "LIBLINEAR"), identical(r$solver_type, 2L),
            identical(r$epsilon, .01), identical(r$seed, 1L),
            identical(r$error, ""), identical(r$degenerate, FALSE),
            identical(r$selection_grid_size, 25L),
            is.data.frame(p), nrow(p) == 1L,
            identical(p$kernel, "rff"), isTRUE(p$weighted),
            p$cost %in% COSTS, p$gamma %in% GAMMAS, identical(p$rff_dim, 128L))
  r
}

# 3. Doc du lieu va tach tap huan luyen / kiem tra
read_window <- function(y) {
  d <- readRDS(input(y))
  X <- as.data.frame(d$X)
  Y <- as.integer(as.character(d$Y))
  ID <- as.character(d$ID)
  yr <- as.integer(format(X$date_versioning, "%Y"))
  stopifnot(nrow(X) == length(Y), length(Y) == length(ID),
            !anyNA(Y), all(Y %in% 0:1), !anyNA(ID), all(nzchar(ID)),
            !anyNA(yr), identical(sort(unique(yr)), 2011:y),
            !anyDuplicated(paste(ID, yr, sep = "|")))
  list(X = X, Y = Y, ID = ID, yr = yr, tr = which(yr < y), te = which(yr == y))
}

# 4. Tien xu ly du lieu chi tinh tren tap Train
prepare <- function(X, tr, te) {
  features <- setdiff(names(X), c("date_versioning", "current_upb"))
  stopifnot(all(names(CATS) %in% features))
  a <- X[tr, features, drop = FALSE]
  b <- X[te, features, drop = FALSE]
  fills <- setNames(vector("list", length(features)), features)
  flags <- character()
  for (nm in features) {
    u <- a[[nm]]; v <- b[[nm]]
    if (nm %in% names(CATS)) {
      lev <- CATS[[nm]]
      u <- trimws(as.character(u)); v <- trimws(as.character(v))
      u[!u %in% lev] <- NA; v[!v %in% lev] <- NA
      fill <- lev[which.max(tabulate(match(u[!is.na(u)], lev), nbins = length(lev)))]
      u[is.na(u)] <- fill; v[is.na(v)] <- fill
      a[[nm]] <- factor(u, levels = lev); b[[nm]] <- factor(v, levels = lev)
    } else {
      stopifnot(is.numeric(u), is.numeric(v))
      u[!is.finite(u)] <- NA; v[!is.finite(v)] <- NA
      if (anyNA(u)) {
        flag <- paste0(nm, "_missing")
        stopifnot(!flag %in% names(a))
        a[[flag]] <- as.integer(is.na(u)); b[[flag]] <- as.integer(is.na(v))
        flags <- c(flags, nm)
      }
      fill <- if (all(is.na(u))) 0 else median(u, na.rm = TRUE)
      u[is.na(u)] <- fill; v[is.na(v)] <- fill
      a[[nm]] <- u; b[[nm]] <- v
    }
    fills[[nm]] <- fill
  }
  A <- model.matrix(~ . - 1, a); B <- model.matrix(~ . - 1, b)
  stopifnot(identical(colnames(A), colnames(B)))
  mu <- colMeans(A); s <- apply(A, 2, sd)
  s[!is.finite(s) | s == 0] <- 1
  A <- scale(A, center = mu, scale = s)
  B <- scale(B, center = mu, scale = s)
  stopifnot(all(is.finite(A)), all(is.finite(B)))
  list(A = A, B = B, meta = list(features = features, fills = fills,
       categories = CATS, missing_indicators = flags, terms = colnames(A),
       center = mu, scale = s, contrasts = getOption("contrasts")))
}

# 5. Ham bien doi RFF
rff <- function(X, W, phase) {
  Z <- matrix(0, nrow(X), ncol(W))
  for (j in seq(1L, ncol(W), by = 16L)) {
    k <- j:min(j + 15L, ncol(W))
    Z[, k] <- sqrt(2 / ncol(W)) *
      cos(sweep(X %*% W[, k, drop = FALSE], 2, phase[k], "+"))
  }
  Z
}

# 6. Huan luyen Weighted RFF-SVC va trich xuat diem du bao
fit_svc <- function(d, best) {
  p <- best$params
  prep <- prepare(d$X, d$tr, d$te)
  meta <- prep$meta
  A <- prep$A; B <- prep$B; rm(prep)
  ytr <- factor(d$Y[d$tr], levels = 0:1)
  stopifnot(all(table(ytr) > 0))

  start <- proc.time()[["elapsed"]]
  set.seed(best$seed)
  W <- matrix(rnorm(ncol(A) * p$rff_dim, sd = sqrt(2 * p$gamma)),
              nrow = ncol(A), ncol = p$rff_dim)
  phase <- runif(p$rff_dim, 0, 2 * pi)
  A <- rff(A, W, phase); B <- rff(B, W, phase)
  meta$rff <- list(W = W, phase = phase, gamma = p$gamma)
  map_sec <- proc.time()[["elapsed"]] - start

  wi <- setNames(as.numeric(length(ytr) / (2 * table(ytr))), levels(ytr))
  set.seed(best$seed)
  elapsed <- system.time({
    model <- LiblineaR(data = A, target = ytr, type = best$solver_type,
                       cost = p$cost, epsilon = best$epsilon, bias = 1,
                       wi = wi, verbose = FALSE)
  })[["elapsed"]]
  rm(A)

  z <- predict(model, B, decisionValues = TRUE)
  first <- as.character(model$ClassNames[1L])
  stopifnot(first %in% c("0", "1"), first %in% colnames(z$decisionValues))
  score <- as.numeric(z$decisionValues[, first])
  if (first == "0") score <- -score
  use <- abs(score) > 1e-9
  stopifnot(length(score) == length(d$te), all(is.finite(score)),
            all(ifelse(score[use] > 0, "1", "0") == as.character(z$predictions[use])),
            diff(range(score)) > 1e-10 * max(1, max(abs(score))))
  list(model = model, prep = meta, score = score, class_weights = wi,
       time_training = elapsed, time_rff = map_sec)
}

# 7. Hieu chuan xac suat bang hoi quy sigmoid
calibrate <- function(score, y) {
  stopifnot(length(unique(y)) == 2L)
  center <- mean(score); spread <- sd(score)
  stopifnot(is.finite(spread), spread > 0)
  z <- (score - center) / spread
  m <- glm(y ~ z, family = binomial())
  beta <- unname(coef(m))
  stopifnot(isTRUE(m$converged), length(beta) == 2L, all(is.finite(beta)), beta[2] > 0)
  list(beta = beta, center = center, scale = spread,
       method = "validation_sigmoid_then_refit")
}

# 8. Hieu chuan, huan luyen lai va du bao cho nam kiem tra
run_year <- function(y, best) {
  v <- y - 1L
  start <- proc.time()[["elapsed"]]
  d <- read_window(v)
  stopifnot(length(d$tr) == best$n_train, length(d$te) == best$n_val,
            sum(d$Y[d$tr]) == best$defaults_train, sum(d$Y[d$te]) == best$defaults_val)
  inner <- fit_svc(d, best)
  cal <- calibrate(inner$score, d$Y[d$te])
  auc <- as.numeric(pROC::auc(pROC::roc(d$Y[d$te], inner$score,
                        levels = c(0, 1), direction = "<", quiet = TRUE)))
  if (abs(auc - best$val_auc) > 1e-5)
    stop("Validation AUC differs from tuning for ", v)
  cal$year <- v
  historical_key <- paste(d$ID, d$yr, sep = "|")
  historical_y <- d$Y
  calibration_seconds <- proc.time()[["elapsed"]] - start
  rm(inner, d); gc(verbose = FALSE)

  d <- read_window(y)
  idx <- match(paste(d$ID[d$tr], d$yr[d$tr], sep = "|"), historical_key)
  stopifnot(length(idx) == length(historical_key), !anyNA(idx),
            identical(d$Y[d$tr], historical_y[idx]))
  final <- fit_svc(d, best)
  pd <- plogis(cal$beta[1L] + cal$beta[2L] * (final$score - cal$center) / cal$scale)
  stopifnot(length(pd) == length(d$te), all(is.finite(pd)), all(pd >= 0 & pd <= 1))

  saveRDS(list(version = VERSION, evaluation_scope = SCOPE, test_year = y, best = best,
               model = final$model, preprocessing = final$prep,
               class_weights = final$class_weights, calibration = cal),
          file.path(root, sprintf("model_SVC_%d.rds", y)))
  meta <- data.frame(year = v, test_year = y, model = "SVC", best$params,
                     time_training = final$time_training, time_rff = final$time_rff,
                     time_calibration = calibration_seconds,
                     n_train = length(d$tr), n_test = length(d$te))
  audit <- data.frame(test_year = y, test_row = seq_along(d$te), loan_id = d$ID[d$te],
                      original_rds_row = d$te, actual_default = d$Y[d$te],
                      decision_score = final$score, pred_response_mean = pd)
  cat(sprintf("Test %d | Job %02d | RFF | C=%g | gamma=%g | train=%.2fs | n_test=%d\n",
              y, best$i_param, best$params$cost, best$params$gamma,
              final$time_training, length(pd)))
  list(prediction = list(pred_response_mean = unname(pd)), meta = meta, audit = audit)
}

# 9. Chay cho 6 nam va luu ket qua du bao
best <- lapply(years, read_best)
results <- Map(run_year, years, best)
predictions <- setNames(lapply(results, `[[`, "prediction"), paste0("pred_year_", years))
attr(predictions, "evaluation_scope") <- SCOPE
parameters <- do.call(rbind, lapply(results, `[[`, "meta"))
saveRDS(predictions, file.path(root, "prediction_SVC.rds"))
saveRDS(parameters, file.path(root, "parameters_SVC.rds"))
write.csv(do.call(rbind, lapply(results, `[[`, "audit")),
          file.path(root, "prediction_alignment_SVC.csv"), row.names = FALSE)
cat("Done: six Weighted RFF-SVC models and predictions saved (", SCOPE, ").\n", sep = "")
