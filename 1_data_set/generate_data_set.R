suppressPackageStartupMessages({ library(data.table); library(lubridate) })
ROOT <- "C:/Users/24521/Downloads/Mortgage-Risk/1_data_set"

DATA <- file.path(ROOT, "data")

RAW <- file.path(DATA, "raw", "freddie_mac")

OUT <- file.path(DATA, "processed", "freddie_mac")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
YEARS <- 2010:2015; SNAPSHOTS <- 2011:2016
RDS_YEARS <- c(2015L, 2016L); WRITE_SPLITS <- TRUE
stopifnot(dir.exists(RAW)); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(x))
recode <- function(x, valid, missing, field) {
  v <- toupper(chr(x)); v <- sub("^0+([0-9])$", "\\1", v)
  unexpected <- unique(v[!is.na(v) & !v %in% c(valid, missing)])
  if (length(unexpected)) stop("Invalid ", field, " values: ", paste(head(unexpected, 12), collapse = ", "))
  v[v %in% missing] <- NA_character_; v
}
chr <- function(x) { x <- trimws(as.character(x)); x[!is.na(x) & x == ""] <- NA_character_; x }
md <- function(x) { v <- chr(x); v[!is.na(v) & !grepl("^[0-9]{6}$", v)] <- NA_character_
as.Date(paste0(v, "01"), "%Y%m%d") }
zip3 <- function(x) {                             # pad numeric ZIP3; preserve 5->3 digit input
  s <- chr(x); out <- rep(NA_character_, length(s))
  ok <- !is.na(s) & grepl("^([0-9]{1,3}|[0-9]{5})$", s)
  out[ok] <- ifelse(nchar(s[ok]) <= 3L,
                    sprintf("%03d", as.integer(s[ok])), substr(s[ok], 1, 3))
  out
}
aux <- function(f) {
  p <- c(file.path(DATA, f), file.path(RAW, f)); p <- p[file.exists(p)]
  if (!length(p)) stop("Missing auxiliary file: ", f); p[1]
}
# For lines ending in '|', strsplit can drop a final empty field. Count pipes instead.
check_schema <- function(path, n) {
  s <- readLines(path, n = 1000L, warn = FALSE)
  if (!length(s)) stop("Empty file: ", path)
  counts <- nchar(s) - nchar(gsub("|", "", s, fixed = TRUE)) + 1L
  if (any(counts != n)) stop("Schema mismatch in ", path,
                             ": expected ", n, "; observed ",
                             paste(unique(counts), collapse = ","))
}
read_sample <- function(y, kind) {
  fname <- sprintf("sample_%s_%d.txt", kind, y)
  path <- file.path(RAW, fname); tmp <- NULL
  if (!file.exists(path)) {
    zips <- list.files(RAW, "\\.zip$", full.names = TRUE, ignore.case = TRUE)
    hits <- lapply(zips, function(z) {
      e <- tryCatch(utils::unzip(z, list = TRUE)$Name,
                    error = function(e) character())
      e <- e[basename(e) == fname]
      if (length(e)) list(zip = z, entry = e[1]) else NULL
    }); hits <- Filter(Negate(is.null), hits)
    if (length(hits) != 1L) stop("Expected exactly one ", fname, " in ZIPs; found ", length(hits))
    tmp <- tempfile("freddie_"); dir.create(tmp)
    utils::unzip(hits[[1]]$zip, files = hits[[1]]$entry, exdir = tmp)
    path <- file.path(tmp, hits[[1]]$entry)
  }
  on.exit(if (!is.null(tmp)) unlink(tmp, recursive = TRUE), add = TRUE)
  is_o <- kind == "orig"; check_schema(path, if (is_o) 31L else 35L)
  idx <- if (is_o) 1:23 else c(1:5, 8, 10, 11)
  nms <- if (is_o) c("credit_score", "date_first_payment", "first_time_homebuyer",
                     "date_maturity", "MSA", "insurance_percent", "nr_units", "occupancy",
                     "original_combined_loan_to_value", "original_dept_to_income", "original_upb",
                     "original_loan_to_value", "original_interest_rate", "channel", "prepayment_penalty",
                     "amortization_type", "property_state", "property_type", "postal_code",
                     "loan_number", "loan_purpose", "loan_term", "number_of_borrowers") else
                       c("loan_number", "period", "current_upb", "delinquency", "loan_age",
                         "modified", "date_zero_balance", "current_interest_rate")
  d <- fread(path, sep = "|", header = FALSE, select = idx,
             colClasses = "character", na.strings = NULL, showProgress = FALSE)
  setnames(d, nms); d[, loan_number := chr(loan_number)]; d[!is.na(loan_number)]
}

# Spatial / macro sources use the same feature definitions as the original code.
post <- fread(aux("ZIP_centroids.csv"), colClasses = list(character = "ZIP3"))
if (!all(c("ZIP3", "X_centroid", "Y_centroid") %in% names(post))) stop("Invalid ZIP_centroids.csv")
post[, `:=`(ZIP3 = zip3(ZIP3), X_centroid = num(X_centroid), Y_centroid = num(Y_centroid))]
post <- unique(post[!is.na(ZIP3) & !ZIP3 %in% c("000", "001") &
                      between(X_centroid, -125, -66, incbounds = FALSE) &
                      between(Y_centroid, 24, 50, incbounds = FALSE),
                    .(ZIP3, X_centroid, Y_centroid)], by = "ZIP3")
if (!nrow(post)) stop("No usable ZIP centroids")
macro <- fread(aux("macro_data.csv"), sep = ";")
if (!all(c("year", "state_short") %in% names(macro)) || ncol(macro) < 4)
  stop("Invalid macro_data.csv")
mac_cols <- names(macro)[4:ncol(macro)]    # exactly original repo's feature selection
macro[, `:=`(year = as.integer(year), state_short = toupper(chr(state_short)))]
for (c in mac_cols) set(macro, j = c, value = num(macro[[c]]))
if (anyDuplicated(macro[, .(year, state_short)])) stop("Duplicate macro year/state")
macro[, macro_key := paste(year, state_short)]
rate <- fread(aux("MORTGAGE30US.csv"))

if ("observation_date" %in% names(rate)) {
  setnames(rate, "observation_date", "DATE")
}

if (!all(c("DATE", "MORTGAGE30US") %in% names(rate))) {
  stop("Invalid MORTGAGE30US.csv")
}

rate[, `:=`(DATE = as.Date(DATE), MORTGAGE30US = num(MORTGAGE30US))]
rate <- rate[!is.na(DATE) & !is.na(MORTGAGE30US)]; setorder(rate, DATE)
if (!nrow(rate)) stop("No usable MORTGAGE30US observations")

orig <- perf <- list(); audit <- list(); borrower_audit <- list(); numeric_audit <- list()
numeric_domains <- list(
  credit_score = c(301, 850, 9999),
  insurance_percent = c(0, 55, 999),
  original_combined_loan_to_value = c(1, 998, 999),
  original_dept_to_income = c(0, 65, 999),
  original_loan_to_value = c(1, 998, 999)
)

legacy_ltv <- c(
  original_combined_loan_to_value = 200,
  original_loan_to_value = 105
)
for (y in YEARS) {
  message("Reading vintage ", y)
  o <- read_sample(y, "orig")
  o <- o[property_type == "SF" & amortization_type == "FRM" & num(loan_term) == 360]
  borrower_audit[[as.character(y)]] <- o[, .N, by = .(raw_value = chr(number_of_borrowers))][
    , vintage := y][]
  numeric_audit[[as.character(y)]] <- rbindlist(lapply(names(numeric_domains), function(nm) {
    r <- numeric_domains[[nm]]; v <- num(o[[nm]])
    data.table(vintage = y, variable = nm, rows = length(v),
               sentinel = sum(v == r[3], na.rm = TRUE), blank = sum(is.na(v)),
               invalid = sum(!is.na(v) & v != r[3] & (v < r[1] | v > r[2])),
               outside_non_relief_limits =
                 if (nm %in% names(legacy_ltv)) {
                   sum(
                     !is.na(v) & v != r[3] &
                       (v < 6 | v > legacy_ltv[[nm]])
                   )
                 } else {
                   0L
                 },
               min_observed = if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE),
               max_observed = if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
  }))
  p <- read_sample(y, "perf")
  matched <- sum(unique(p$loan_number) %chin% o$loan_number)
  audit[[as.character(y)]] <- data.table(vintage = y, eligible = nrow(o),
                                         perf_unique_loans = uniqueN(p$loan_number), matched = matched)
  p <- p[loan_number %chin% o$loan_number & as.integer(period) < 201701L]
  orig[[as.character(y)]] <- o; perf[[as.character(y)]] <- p
}
o <- rbindlist(orig); p <- rbindlist(perf); rm(orig, perf); gc(FALSE)
if (!nrow(o) || !nrow(p) || anyDuplicated(o$loan_number)) stop("Empty data or duplicate loan_number")
fwrite(rbindlist(audit), file.path(OUT, "vintage_audit.csv"))
fwrite(rbindlist(borrower_audit)[, .(vintage, raw_value, N)],
       file.path(OUT, "borrower_domain_audit.csv"), na = "")
naudit <- rbindlist(numeric_audit)
fwrite(naudit, file.path(OUT, "numeric_domain_audit.csv"))
if (any(naudit$invalid > 0)) stop("Numeric domain violations. Check numeric_domain_audit.csv")

# ZIP audit separates invalid (000/001/NA) from valid-but-unmatched regions.
o[, ZIP3 := zip3(postal_code)]
zip_eligible <- !is.na(o$ZIP3) & !o$ZIP3 %chin% c("000", "001")
zip_ok <- o$ZIP3 %chin% post$ZIP3
zip_audit <- data.table(total = nrow(o), unknown_000 = sum(o$ZIP3 == "000", na.rm=TRUE),
                        distributed_001 = sum(o$ZIP3 == "001", na.rm=TRUE), missing_zip = sum(is.na(o$ZIP3)),
                        eligible = sum(zip_eligible), matched = sum(zip_eligible & zip_ok),
                        eligible_unmatched = sum(zip_eligible & !zip_ok),
                        eligible_match_rate = if (any(zip_eligible)) mean(zip_ok[zip_eligible]) else NA_real_)
# Keep earlier audit for drift comparison; thresholds are warnings, not paper rules.
zip_path <- file.path(OUT, "zip3_join_audit.csv")
if (file.exists(zip_path)) {
  previous <- tryCatch(fread(zip_path), error = function(e) NULL)
  if (!is.null(previous) && "eligible_match_rate" %in% names(previous) && nrow(previous) &&
      is.finite(previous$eligible_match_rate[1]) &&
      is.finite(zip_audit$eligible_match_rate) &&
      zip_audit$eligible_match_rate < previous$eligible_match_rate[1] - 0.10)
    warning("ZIP3 eligible match rate fell by >10 percentage points from previous run")
}
fwrite(zip_audit, zip_path)
if (is.finite(zip_audit$eligible_match_rate) && zip_audit$eligible_match_rate < 0.50)
  warning("ZIP3 eligible match rate <50%; inspect centroid version and coverage")
message(sprintf("ZIP3 eligible match: %.2f%% (%d/%d)",
                100*zip_audit$eligible_match_rate, zip_audit$matched, zip_audit$eligible))
if (!is.finite(zip_audit$eligible_match_rate)) stop("No eligible ZIP3 to join")
# No arbitrary 80%% hard stop: excluded non-mainland ZIPs are expected.
if (!any(zip_ok)) stop("No origination ZIP matched ZIP_centroids.csv")
o <- merge(o, post, by = "ZIP3", sort = FALSE)
p <- p[loan_number %chin% o$loan_number]

# Only loans with an unmodified observation in their first 0-3 loan-age months.
p[, `:=`(modified = toupper(chr(modified)), loan_age = num(loan_age))]
if (any(!is.na(p$modified) & !p$modified %in% c("Y", "P")))
  stop("Unexpected modified flag; inspect unique(p$modified)")
initial <- unique(p[loan_age %between% c(0, 3) & is.na(modified), loan_number])
o <- o[loan_number %chin% initial]; p <- p[loan_number %chin% o$loan_number]
if (!nrow(o)) stop("All loans excluded by 0-3 month performance criterion")

# Preserve origination LTV (column 12) before replacing 999 sentinel with NA.
o[, original_loan_to_value_raw := num(original_loan_to_value)]
# Sentinel handling only; imputation on model TRAIN (avoid leakage).
for (nm in c("credit_score", "insurance_percent", "original_dept_to_income",
             "original_combined_loan_to_value", "original_loan_to_value")) {
  v <- num(o[[nm]]); missing_code <- if (nm == "credit_score") 9999 else
    if (nm == "insurance_percent") 999 else 999
  v[!is.na(v) & v == missing_code] <- NA_real_; set(o, j = nm, value = v)
}
for (nm in c("original_upb", "original_interest_rate")) set(o, j = nm, value = num(o[[nm]]))
# Field-specific missing codes and domains; do NOT coerce unexpected values.
valid_categories <- list(nr_units = c("1","2","3","4"),
                         occupancy = c("P","I","S"), loan_purpose = c("P","C","N","R"),
                         first_time_homebuyer = c("Y","N"), channel = c("R","B","C","T"),
                         number_of_borrowers = c("1","2"))
for (nm in names(valid_categories)) {
  missing <- if (nm %in% c("nr_units", "number_of_borrowers")) "99" else "9"
  set(o, j = nm, value = recode(o[[nm]], valid_categories[[nm]], missing, nm))
}
# Preserve original paper's Y/N -> 1/0 coding, not other category levels.
o[first_time_homebuyer == "N", first_time_homebuyer := "0"]
o[first_time_homebuyer == "Y", first_time_homebuyer := "1"]
o[, MSA := fifelse(is.na(chr(MSA)), "0", "1")]
o[, property_state := toupper(chr(property_state))]
cat_cols <- names(valid_categories)
cat_audit <- rbindlist(lapply(cat_cols, function(nm) o[, .(
  field = nm, total = .N, missing = sum(is.na(get(nm))),
  missing_rate = mean(is.na(get(nm))))]))
fwrite(cat_audit, file.path(OUT, "categorical_missing_audit.csv"))


# First 90+ delinquency event; RA / XX excluded, not silently classified default.
p[, delinquency := toupper(chr(delinquency))]
unknown_status <- unique(p[!is.na(delinquency) &
                             !grepl("^[0-9]{1,2}$", delinquency) & !delinquency %chin% c("RA", "XX"), delinquency])
if (length(unknown_status)) stop("Unknown delinquency codes: ", paste(unknown_status, collapse = ", "))
p[, `:=`(period_date = md(period), delinquency_num = suppressWarnings(as.integer(delinquency)),
         zero_date = md(date_zero_balance), current_upb = num(current_upb),
         current_interest_rate = num(current_interest_rate))]
message("Non-numeric delinquency statuses:")
print(p[!is.na(chr(delinquency)) & is.na(delinquency_num), .N, by = delinquency][order(-N)])
d <- p[!is.na(delinquency_num) & delinquency_num >= 3L & !is.na(period_date),
       .(date_default = min(period_date)), by = loan_number]
ra <- p[delinquency == "RA" & !is.na(period_date),
        .(first_ra = min(period_date), ra_rows = .N), by = loan_number]
ra <- merge(ra, d, by = "loan_number", all.x = TRUE, sort = FALSE)
ra[, prior_90plus := !is.na(date_default) & date_default < first_ra]
ra[, flag_ra_without_prior_90plus := !prior_90plus]
fwrite(ra, file.path(OUT, "ra_loan_audit.csv"))
message(sprintf("RA audit: %d loans with RA; %d without earlier numeric 03+",
                nrow(ra), sum(ra$flag_ra_without_prior_90plus)))
z <- p[!is.na(zero_date), .(date_zero_balance = min(zero_date)), by = loan_number]
o <- merge(merge(o, d, by = "loan_number", all.x = TRUE, sort = FALSE), z,
           by = "loan_number", all.x = TRUE, sort = FALSE)
o[, `:=`(date_first_payment = md(date_first_payment), date_maturity = md(date_maturity))]
o[, date_loan_start := date_first_payment %m-% months(1)]
o[, date_loan_end := pmin(fcoalesce(date_default, as.Date("9999-12-01")),
                          fcoalesce(date_zero_balance, as.Date("9999-12-01")),
                          fcoalesce(date_maturity, as.Date("9999-12-01")))]
o <- o[!is.na(date_loan_start) & !is.na(date_loan_end) &
         date_loan_end != as.Date("9999-12-01")]
o[, original_value := fifelse(!is.na(original_loan_to_value) & original_loan_to_value > 0,
                              original_upb / original_loan_to_value * 100, NA_real_)]
p <- p[loan_number %chin% o$loan_number & !is.na(period_date)]
setorder(p, loan_number, period_date)

features <- c("X_centroid", "Y_centroid", "credit_score", "occupancy", "nr_units",
              "loan_purpose", "first_time_homebuyer", "MSA", "insurance_percent",
              "original_dept_to_income", "original_combined_loan_to_value",
              "original_upb", "number_of_borrowers")
rows <- list(); macro_audit <- list()
for (y in SNAPSHOTS) {
  date <- as.Date(sprintf("%d-01-01", y)); next_date <- date %m+% years(1)
  a <- o[date_loan_start < date & date_loan_end >= date]
  if (!nrow(a)) stop("No active loans at snapshot ", y)
  latest <- p[loan_number %chin% a$loan_number & period_date < date,
              .SD[.N], by = loan_number,
              .SDcols = c("period_date", "current_upb", "current_interest_rate")]
  latest <- latest[match(a$loan_number, latest$loan_number)]
  new <- is.na(latest$period_date)
  latest$period_date[new] <- a$date_loan_start[new]
  latest$current_upb[new] <- a$original_upb[new]
  latest$current_interest_rate[new] <- a$original_interest_rate[new]
  fresh <- !is.na(latest$period_date) & latest$period_date >= date %m-% months(6)
  a <- a[fresh]; latest <- latest[fresh]
  if (!nrow(a)) stop("No fresh active loans at snapshot ", y)
  available_rate <- rate[DATE < date, MORTGAGE30US]
  if (!length(available_rate)) stop("No mortgage rates before ", date)
  im <- match(paste(y - 1L, a$property_state), macro$macro_key)
  if (anyNA(im)) stop("Macro mapping missing for ", y, ": ", sum(is.na(im)), " rows")
  X <- copy(a[, ..features]); X[, date_versioning := date]
  X[, `:=`(current_loan_to_value = latest$current_upb / a$original_value * 100,
           ir_spread = latest$current_interest_rate - tail(available_rate, 1),
           n_months = interval(a$date_loan_start, date) %/% months(1) - 1L,
           current_upb = latest$current_upb)]
  for (c in mac_cols) X[, (c) := macro[[c]][im]]
  mac_stat <- rbindlist(lapply(mac_cols, function(nm) X[, .(
    year = y, variable = nm, rows = .N, missing = sum(is.na(get(nm))),
    missing_rate = mean(is.na(get(nm))))]))
  macro_audit[[as.character(y)]] <- mac_stat
  if (any(mac_stat$missing == mac_stat$rows))
    stop("Entirely missing macro variable(s) at snapshot ", y, ": ",
         paste(mac_stat[missing == rows, variable], collapse = ", "))
  Y <- as.integer(!is.na(a$date_default) & a$date_default < next_date)
  result <- data.table(loan_id = a$loan_number, default = Y, X,
                       original_loan_to_value_aux = a$original_loan_to_value_raw)
  # AUX holds column-12 raw LTV including sentinel 999; not a predictor.
  if (anyDuplicated(result[, .(loan_id, date_versioning)]) || anyNA(Y) || !all(Y %in% 0:1))
    stop("Invalid loan-year keys or target in ", y)
  rows[[as.character(y)]] <- result
  message(sprintf("Snapshot %d: %s rows; %s defaults", y, nrow(result), sum(Y)))
  if (y %in% RDS_YEARS) {
    w <- rbindlist(rows)
    xf <- as.data.frame(w[, !c("loan_id", "default", "original_loan_to_value_aux"), with = FALSE])
    for (c in c("occupancy", "nr_units", "loan_purpose", "first_time_homebuyer",
                "MSA", "number_of_borrowers")) xf[[c]] <- factor(xf[[c]])
    v <- w$original_loan_to_value_aux
    stopifnot(nrow(xf) == length(w$default), length(w$default) == length(w$loan_id),
              length(w$loan_id) == length(v))
    saveRDS(list(Y = w$default, X = xf, ID = w$loan_id,
                 AUX = list(original_loan_to_value_raw = v)),
            file.path(OUT, sprintf("data_window_%d.rds", y)), compress = "xz")
  }
}
master <- rbindlist(rows)
fwrite(rbindlist(macro_audit), file.path(OUT, "macro_missing_audit.csv"))
fwrite(master, file.path(OUT, "loan_year_master.csv"), na = "")
if (WRITE_SPLITS) {
  sd <- file.path(OUT, "splits"); dir.create(sd, showWarnings = FALSE)
  yr <- year(master$date_versioning)
  for (nm in c("train", "validation", "test")) {
    ii <- switch(nm, train = yr %in% 2011:2014, validation = yr == 2015, test = yr == 2016)
    fwrite(master[ii], file.path(sd, paste0(nm, ".csv")), na = "")
  }
}
quality <- master[, .(rows = .N, defaults = sum(default), default_rate = mean(default),
                      missing_ltv = sum(is.na(current_loan_to_value))), by = .(year = year(date_versioning))]
fwrite(quality, file.path(OUT, "data_quality_by_year.csv")); print(quality)
message("DONE. RDS windows + master + splits + audits created.")