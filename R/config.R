# Shared project configuration.
PROJECT_ROOT <- normalizePath(".", winslash = "/", mustWork = FALSE)
YEARS <- list(train = 2011:2014, validation = 2015, test = 2016)
EXTERNAL_VALIDATION_MODELS <- c("LightGBM", "GPBoost_spatial")
