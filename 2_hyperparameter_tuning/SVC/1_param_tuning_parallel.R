library(LiblineaR)
library(pROC)

VERSION <- "svc_weighted_rff"
SCOPE <- "retrospective_exploratory"
EPS <- 0.01
D <- 128L
SEED <- 1L

if (!exists("val_year")) stop("Set val_year in 2016:2021 before sourcing.")
yr <- as.integer(val_year)
stopifnot(length(yr) == 1L, !is.na(yr), yr %in% 2016:2021)

# 1. Khoi tao luoi 25 to hop Weighted RFF-SVC
grid <- expand.grid(
  kernel = "rff",
  cost = c(.001, .003, .01, .03, .1),
  gamma = c(1e-4, 3e-4, .001, .003, .01),
  weighted = TRUE,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
grid$i_param <- seq_len(nrow(grid))
grid$rff_dim <- D

# 2. Kiem tra duong dan va bo qua to hop da hoan thanh
input <- file.path("1_data_set/data/processed/freddie_mac", sprintf("data_window_%d.rds", yr))
out <- file.path("2_hyperparameter_tuning/SVC", paste0("data_", yr))
stopifnot(file.exists(input))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

md5 <- unname(tools::md5sum(input))
pkg_version <- as.character(packageVersion("LiblineaR"))
result_path <- function(i) file.path(out, sprintf("param_comb_%02d.rds", i))

pending <- which(vapply(seq_len(nrow(grid)), function(i) {
  f <- result_path(i)
  if (!file.exists(f)) return(TRUE)
  r <- tryCatch(readRDS(f), error = function(e) NULL)
  valid <- is.list(r) &&
    identical(r$version, VERSION) && identical(r$evaluation_scope, SCOPE) &&
    identical(r$data_md5, md5) && identical(r$package_version, pkg_version) &&
    identical(r$epsilon, EPS) && identical(r$seed, SEED) &&
    identical(r$val_year, yr) && identical(r$params, grid[i, , drop = FALSE]) &&
    identical(r$error, "") && length(r$val_auc) == 1L && is.finite(r$val_auc)
  if (!valid) stop("Incompatible result in ", f)
  cat(sprintf("SKIP Job %02d/25 (C=%g, gamma=%g)\n", i, grid$cost[i], grid$gamma[i]))
  FALSE
}, logical(1)))

if (!length(pending)) {
  cat("Nothing to run for validation year", yr, "\n")
} else {
  # 3. Doc du lieu va chia Expanding Window
  d <- readRDS(input)
  y <- as.integer(as.character(d$Y))
  years <- as.integer(format(d$X$date_versioning, "%Y"))
  stopifnot(length(y) == nrow(d$X), !anyNA(y), all(y %in% 0:1),
            !anyNA(years), identical(sort(unique(years)), 2011:yr))

  tr <- which(years < yr)
  va <- which(years == yr)
  features <- setdiff(names(d$X), c("date_versioning", "current_upb"))
  a <- d$X[tr, features, drop = FALSE]
  b <- d$X[va, features, drop = FALSE]
  ytr <- factor(y[tr], levels = 0:1)
  yva <- y[va]
  stopifnot(all(table(ytr) > 0), length(unique(yva)) == 2L)
  rm(d)

  # 4. Dien khuyet bang Median/Mode va tao co thieu tren Train
  CATS <- list(occupancy = c("P", "S", "I"), nr_units = as.character(1:4),
               loan_purpose = c("C", "N", "P"), first_time_homebuyer = c("0", "1"),
               MSA = c("0", "1"), number_of_borrowers = c("1", "2"))
  stopifnot(all(names(CATS) %in% features))

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
      }
      fill <- if (all(is.na(u))) 0 else median(u, na.rm = TRUE)
      u[is.na(u)] <- fill; v[is.na(v)] <- fill
      a[[nm]] <- u; b[[nm]] <- v
    }
  }

  # 5. Ma hoa bien phan loai, chuan hoa va tinh trong so lop tren Train
  Xtr <- model.matrix(~ . - 1, a)
  Xva <- model.matrix(~ . - 1, b)
  stopifnot(identical(colnames(Xtr), colnames(Xva)))
  rm(a, b)

  mu <- colMeans(Xtr)
  s <- apply(Xtr, 2, sd)
  s[!is.finite(s) | s == 0] <- 1
  Xtr <- scale(Xtr, center = mu, scale = s)
  Xva <- scale(Xva, center = mu, scale = s)
  stopifnot(all(is.finite(Xtr)), all(is.finite(Xva)))

    wi <- setNames(as.numeric(length(ytr) / (2 * table(ytr))), levels(ytr))
  cat(sprintf("\n Val=%d | Train=%d (def=%d) | Val=%d (def=%d) | Features=%d\n",
              yr, length(ytr), sum(ytr == "1"), length(yva), sum(yva == 1), ncol(Xtr)))

  # 6. Ham bien doi RFF va tinh Average Precision
  rff <- function(X, W, phase) {
    Z <- matrix(0, nrow(X), ncol(W))
    for (j in seq(1L, ncol(W), by = 16L)) {
      k <- j:min(j + 15L, ncol(W))
      Z[, k] <- sqrt(2 / ncol(W)) *
        cos(sweep(X %*% W[, k, drop = FALSE], 2, phase[k], "+"))
    }
    Z
  }

  average_precision <- function(y_true, score) {
    o <- order(score, decreasing = TRUE)
    k <- cumsum(rle(score[o])$lengths)
    tp <- cumsum(y_true[o])[k]
    sum(diff(c(0, tp)) * tp / k) / sum(y_true)
  }

  # 7. Huan luyen, danh gia va luu ket qua mot to hop
  fit_job <- function(i, A, B, map_seconds) {
    p <- grid[i, , drop = FALSE]
    set.seed(SEED)
    cat(sprintf("\nJob %02d/25 | RFF | C=%g | gamma=%g | weighted=TRUE\n", i, p$cost, p$gamma))
    flush.console()

    start <- proc.time()[["elapsed"]]
    model <- LiblineaR(data = A, target = ytr, type = 2L, cost = p$cost,
                       epsilon = EPS, bias = 1, wi = wi, verbose = FALSE)
    pred <- predict(model, B, decisionValues = TRUE)

    first <- as.character(model$ClassNames[1L])
    stopifnot(first %in% c("0", "1"), first %in% colnames(pred$decisionValues))
    score <- as.numeric(pred$decisionValues[, first])
    if (first == "0") score <- -score
    use <- abs(score) > 1e-9
    stopifnot(length(score) == length(yva), all(is.finite(score)),
              all(ifelse(score[use] > 0, "1", "0") == as.character(pred$predictions[use])))

    auc <- as.numeric(pROC::auc(pROC::roc(yva, score, levels = c(0, 1), direction = "<", quiet = TRUE)))
    ap <- average_precision(yva, score)
    span <- diff(range(score))
    degenerate <- span <= 1e-10 * max(1, max(abs(score)))
    elapsed <- proc.time()[["elapsed"]] - start

    result <- list(
      version = VERSION, evaluation_scope = SCOPE, val_year = yr, test_year = yr + 1L,
      i_param = i, params = p, solver = "LIBLINEAR", solver_type = 2L, epsilon = EPS,
      val_auc = auc, val_ap = ap, score_sd = sd(score), score_span = span,
      degenerate = degenerate, elapsed_sec = elapsed, rff_seconds = map_seconds,
      n_train = length(ytr), n_val = length(yva),
      defaults_train = sum(ytr == "1"), defaults_val = sum(yva == 1),
      n_features = ncol(A), class_weights = wi, data_md5 = md5,
      seed = SEED, package_version = pkg_version, error = ""
    )

    tmp <- tempfile("svc_", tmpdir = out)
    saveRDS(result, tmp)
    f <- result_path(i)
    if (file.exists(f) && !file.remove(f)) stop("Cannot replace ", f)
    if (!file.rename(tmp, f)) stop("Cannot save ", f)
    cat(sprintf("AUC=%.6f | AP=%.6f | SD=%.4g | %.1fs%s\n",
                auc, ap, sd(score), elapsed, if (degenerate) " | NEAR-CONSTANT" else ""))
    flush.console()
  }

  # 8. Tao ma tran RFF theo tung gamma va chay qua cac gia tri C
  for (g in unique(grid$gamma[pending])) {
    ids <- pending[grid$gamma[pending] == g]
    start <- proc.time()[["elapsed"]]
    set.seed(SEED)
    W <- matrix(rnorm(ncol(Xtr) * D, sd = sqrt(2 * g)), nrow = ncol(Xtr), ncol = D)
    phase <- runif(D, 0, 2 * pi)
    A <- rff(Xtr, W, phase)
    B <- rff(Xva, W, phase)
    map_seconds <- proc.time()[["elapsed"]] - start
    cat("RFF gamma =", g, "| Dimension =", D, "\n")

    for (i in ids) fit_job(i, A, B, map_seconds)
    rm(A, B)
    gc(verbose = FALSE)
  }

  cat("\nWeighted RFF-SVC tuning completed for validation year", yr, "\n")
}
