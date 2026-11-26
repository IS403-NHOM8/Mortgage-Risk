root <- file.path("2_hyperparameter_tuning", "SVC")
data_dir <- file.path("1_data_set", "data", "processed", "freddie_mac")
val_years <- 2016:2021
VERSION <- "svc_weighted_rff"
SCOPE <- "retrospective_exploratory"
EPS <- 0.01
D <- 128L
SEED <- 1L
pkg_version <- as.character(packageVersion("LiblineaR"))

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

scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
unit <- function(x) scalar(x) && x >= 0 && x <= 1
nonneg <- function(x) scalar(x) && x >= 0
count <- function(x) nonneg(x) && x == floor(x)

# 2. Kiem tra tinh hop le cua tung ket qua tuning
check_job <- function(i, y, folder, md5) {
  path <- file.path(folder, sprintf("param_comb_%02d.rds", i))
  fail <- function(msg) stop(path, ": ", msg, call. = FALSE)
  if (!file.exists(path)) fail("missing tuning result")
  r <- tryCatch(readRDS(path), error = function(e) fail(conditionMessage(e)))
  p <- grid[i, , drop = FALSE]

  if (!is.list(r) || !identical(r$version, VERSION) ||
      !identical(r$evaluation_scope, SCOPE) ||
      !identical(r$val_year, y) || !identical(r$test_year, y + 1L) ||
      !identical(r$i_param, i) || !identical(r$data_md5, md5) ||
      !identical(r$solver, "LIBLINEAR") || !identical(r$solver_type, 2L) ||
      !identical(r$epsilon, EPS) || !identical(r$seed, SEED) ||
      !identical(r$package_version, pkg_version) || !identical(r$error, ""))
    fail("wrong version, scope, year, data, solver or failed job")

  if (!is.data.frame(r$params) || !identical(r$params, p))
    fail("grid parameters do not match tuning script")

  if (!unit(r$val_auc) || !unit(r$val_ap) ||
      !nonneg(r$elapsed_sec) || !nonneg(r$rff_seconds) ||
      !nonneg(r$score_sd) || !nonneg(r$score_span) ||
      !is.logical(r$degenerate) || length(r$degenerate) != 1L || is.na(r$degenerate))
    fail("invalid metrics")

  if (!all(vapply(list(r$n_train, r$n_val, r$defaults_train,
                       r$defaults_val, r$n_features), count, logical(1))) ||
      r$defaults_train <= 0 || r$defaults_train >= r$n_train ||
      r$defaults_val <= 0 || r$defaults_val >= r$n_val ||
      r$n_features != D)
    fail("invalid split or feature dimensions")

  wi <- setNames(r$n_train / (2 * c(r$n_train - r$defaults_train, r$defaults_train)), c("0", "1"))
  if (!isTRUE(all.equal(r$class_weights, wi))) fail("invalid class weights")
  r
}

# 3. Tong hop va chon to hop tot nhat theo tung nam
read_year <- function(y) {
  folder <- file.path(root, paste0("data_", y))
  input <- file.path(data_dir, sprintf("data_window_%d.rds", y))
  if (!file.exists(input)) stop("Missing input: ", input, call. = FALSE)
  md5 <- unname(tools::md5sum(input))
  if (is.na(md5)) stop("Cannot hash input: ", input, call. = FALSE)

  results <- lapply(seq_len(nrow(grid)), check_job, y = y, folder = folder, md5 = md5)

  fields <- c("n_train", "n_val", "defaults_train", "defaults_val", "version",
              "evaluation_scope", "data_md5", "seed", "solver_type", "epsilon", "package_version")
  ref <- results[[1L]]
  if (!all(vapply(results, function(r) identical(r[fields], ref[fields]), logical(1))))
    stop("Inconsistent split or configuration in ", y, call. = FALSE)

  summary <- do.call(rbind, lapply(results, function(r) {
    data.frame(val_year = y, test_year = y + 1L, r$params,
               val_auc = r$val_auc, val_ap = r$val_ap,
               elapsed_sec = r$elapsed_sec, rff_seconds = r$rff_seconds,
               score_sd = r$score_sd, degenerate = r$degenerate)
  }))
  summary <- summary[order(summary$degenerate, -summary$val_auc, -summary$val_ap, summary$i_param), ]
  rownames(summary) <- NULL

  eligible <- summary[!summary$degenerate, , drop = FALSE]
  if (!nrow(eligible)) stop("No eligible Weighted RFF-SVC in ", y, call. = FALSE)

  winner <- eligible[1L, , drop = FALSE]
  best <- results[[winner$i_param]]
  best$selection_metric <- "ROC-AUC"
  best$tie_breakers <- c("higher Average Precision", "smaller combination index")
  best$selection_rule <- "annual best among 25 weighted RFF jobs; exclude near-constant scores"
  best$selection_grid_size <- nrow(grid)

  cat(sprintf("Checked %d: %d/25 | Best job=%02d | C=%g | gamma=%g | AUC=%.6f\n",
              y, length(results), best$i_param, best$params$cost, best$params$gamma, best$val_auc))
  list(year = y, folder = folder, summary = summary, best = best, winner = winner)
}

selected <- lapply(val_years, read_year)

# 4. Luu ket qua tot nhat theo tung nam
for (s in selected) {
  y <- s$year
  write.csv(s$summary, file.path(s$folder, sprintf("tuning_summary_%d.csv", y)), row.names = FALSE)
  saveRDS(s$best, file.path(s$folder, sprintf("best_param_comb_%d.rds", y)))
  saveRDS(s$best$params, file.path(s$folder, sprintf("best_params_%d.rds", y)))
}

# 5. Luu bang tong hop tat ca cac nam
all_results <- do.call(rbind, lapply(selected, `[[`, "summary"))
best_all <- do.call(rbind, lapply(selected, `[[`, "winner"))
rownames(all_results) <- NULL
rownames(best_all) <- NULL
write.csv(all_results, file.path(root, "tuning_summary_all_years.csv"), row.names = FALSE)
write.csv(best_all, file.path(root, "best_params_all_years.csv"), row.names = FALSE)
print(best_all, row.names = FALSE)
cat(sprintf("Done: %d combinations checked; %d annual winners saved (%s).\n",
            nrow(all_results), nrow(best_all), SCOPE))
