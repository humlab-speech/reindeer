#' Optimized version of dspp_metadataParameters using data.table
#'
#' This function returns age and gender specific speech signal processing
#' parameters with significantly improved performance using data.table.
#'
#' @param recompute If `TRUE`, this function will estimate age appropriate
#'   settings from the collection of empirical defaults stored in this package.
#'   If `FALSE`, this function will just return the stored `DSPP` data.
#' @param id.columns Identification columns (default: c("Age","Gender"))
#' @param impute Impute missing settings using time series analysis?
#' @param defaultsEstimatedSampleSize The estimated sample size on which software
#'   default settings have been based.
#'
#' @return A data.table with one row per age and gender combination
#'
#' @importFrom data.table data.table setDT setkey setkeyv := .SD .N .I .GRP
#' @importFrom data.table setnames setcolorder rbindlist dcast
dspp_metadataParameters_dt <- function(recompute=FALSE,
                                        id.columns=c("Age","Gender"),
                                        impute=TRUE,
                                        defaultsEstimatedSampleSize=10){

  # Load data.table if not already loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for this optimized version")
  }

  if(!recompute){
    data(DSPP, package="reindeer")
    return(reindeer::DSPP)
  }

  # Read data once
  defaults <- openxlsx::read.xlsx(
    file.path(system.file(package = "reindeer", mustWork = TRUE),
              "default_parameters.xlsx"),
    sep.names = " "
  )

  # Convert to data.table for efficient operations
  defaults <- data.table::as.data.table(defaults)

  # Pre-compute minimum sample size enforcement (in place)
  defaults[`Study participants` < defaultsEstimatedSampleSize,
           `Study participants` := defaultsEstimatedSampleSize]

  # Process Male and Female data
  DSPP_mf <- process_gender_data(defaults, c("Male", "Female"),
                                  impute, defaultsEstimatedSampleSize)

  # Process Unspecified gender data
  DSPP_unspecified <- process_unspecified_data(defaults, impute,
                                                defaultsEstimatedSampleSize)

  # Combine results efficiently
  DSPP <- data.table::rbindlist(list(DSPP_mf, DSPP_unspecified), use.names=TRUE)

  # Final conversions
  num_cols <- names(DSPP)[sapply(DSPP, is.numeric)]
  DSPP[, (num_cols) := lapply(.SD, as.integer), .SDcols = num_cols]
  DSPP[, Gender := factor(Gender, levels = c("Female", "Male", "Unspecified"))]

  # Convert back to tibble if needed to maintain compatibility
  DSPP <- tibble::as_tibble(DSPP)

  return(DSPP)
}


#' Process gender-specific data with data.table
#' @keywords internal
process_gender_data <- function(defaults, genders, impute, defaultsEstimatedSampleSize) {

  # Filter and select relevant columns
  dt <- defaults[Gender %in% genders,
                 .(Gender, Age_lower, Age_upper, Parameter, Setting,
                   `Study participants`, `Study identifier`)]

  # Expand age ranges efficiently - ensure consistent types
  dt_expanded <- dt[, {
    age_range <- seq(as.integer(Age_lower[1]), as.integer(Age_upper[1]), 1L)
    list(Age = age_range)
  }, by = .(Gender, Parameter, Setting, `Study participants`, `Study identifier`)]

  # Calculate weights efficiently
  dt_expanded[, weight := data.table::fcase(
    grepl("^min", Parameter), `Study participants` / Setting,
    grepl("^max", Parameter), `Study participants` * Setting,
    default = `Study participants`
  )]

  # Remove Study identifier (not needed for modeling)
  dt_expanded[, `Study identifier` := NULL]

  # Floor Age
  dt_expanded[, Age := floor(Age)]

  # Apply loess smoothing by Gender and Parameter
  dt_smoothed <- dt_expanded[, {
    age_seq <- seq(min(Age), max(Age), 1)
    loess_model <- loess(Setting ~ Age, weights = weight, data = .SD, span = 1)
    predicted <- predict(loess_model,
                        data.frame(Age = age_seq),
                        surface = "direct",
                        statistics = "approximate")
    .(Age = age_seq, Setting = predicted)
  }, by = .(Gender, Parameter)]

  # Pivot wider efficiently
  DSPP_wide <- data.table::dcast(dt_smoothed, Gender + Age ~ Parameter, value.var = "Setting")

  # Compute derived columns
  if ("windowSize" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(windowSize) & !is.na(minF),
              windowSize := ceiling(2*1*1000/minF)]
  }
  if ("nominalF2" %in% names(DSPP_wide) && "nominalF1" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(nominalF2), nominalF2 := ceiling(nominalF1*3)]
  }
  if ("nominalF3" %in% names(DSPP_wide) && "nominalF1" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(nominalF3), nominalF3 := ceiling(nominalF1*5)]
  }

  # Round all numeric columns
  num_cols <- names(DSPP_wide)[sapply(DSPP_wide, is.numeric)]
  DSPP_wide[, (num_cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = num_cols]

  # Sort
  data.table::setkey(DSPP_wide, Gender, Age)

  # Impute if requested
  if(impute) {
    # Group by Gender and impute
    DSPP_wide[, (num_cols) := lapply(.SD, function(x) {
      if(any(is.na(x)) && sum(!is.na(x)) > 2) {  # Need at least 3 non-NA values
        tryCatch({
          # Suppress convergence warnings - fallback handles failures
          suppressWarnings(
            imputeTS::na_kalman(x, smooth=FALSE, model="StructTS")
          )
        }, error = function(e) {
          # Fallback to simple imputation if Kalman fails
          imputeTS::na_interpolation(x, option = "linear")
        })
      } else {
        x
      }
    }), .SDcols = num_cols, by = Gender]
  }

  return(DSPP_wide)
}


#' Process unspecified gender data with data.table
#' @keywords internal
process_unspecified_data <- function(defaults, impute, defaultsEstimatedSampleSize) {

  # Select relevant columns (no Gender)
  dt <- defaults[, .(Age_lower, Age_upper, Parameter, Setting,
                     `Study participants`, `Study identifier`)]

  # Expand age ranges efficiently - ensure consistent types
  dt_expanded <- dt[, {
    age_range <- seq(as.integer(Age_lower[1]), as.integer(Age_upper[1]), 1L)
    list(Age = age_range)
  }, by = .(Parameter, Setting, `Study participants`, `Study identifier`)]

  # Calculate weights efficiently
  dt_expanded[, weight := data.table::fcase(
    grepl("^min", Parameter), `Study participants` / Setting,
    grepl("^max", Parameter), `Study participants` * Setting,
    default = `Study participants`
  )]

  # Remove Study identifier
  dt_expanded[, `Study identifier` := NULL]

  # Floor Age
  dt_expanded[, Age := floor(Age)]

  # Apply loess smoothing by Parameter
  dt_smoothed <- dt_expanded[, {
    age_seq <- seq(min(Age), max(Age), 1)
    loess_model <- loess(Setting ~ Age, weights = weight, data = .SD, span = 1)
    predicted <- predict(loess_model,
                        data.frame(Age = age_seq),
                        surface = "direct",
                        statistics = "approximate")
    .(Age = age_seq, Setting = predicted)
  }, by = .(Parameter)]

  # Pivot wider efficiently
  DSPP_wide <- data.table::dcast(dt_smoothed, Age ~ Parameter, value.var = "Setting")

  # Compute derived columns
  if ("windowSize" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(windowSize) & !is.na(minF),
              windowSize := ceiling(2*1*1000/minF)]
  }
  if ("nominalF2" %in% names(DSPP_wide) && "nominalF1" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(nominalF2), nominalF2 := ceiling(nominalF1*3)]
  }
  if ("nominalF3" %in% names(DSPP_wide) && "nominalF1" %in% names(DSPP_wide)) {
    DSPP_wide[is.na(nominalF3), nominalF3 := ceiling(nominalF1*5)]
  }

  # Round all numeric columns
  num_cols <- names(DSPP_wide)[sapply(DSPP_wide, is.numeric)]
  DSPP_wide[, (num_cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = num_cols]

  # Add Gender column
  DSPP_wide[, Gender := "Unspecified"]

  # Sort
  data.table::setkey(DSPP_wide, Age)

  # Impute if requested
  if(impute) {
    DSPP_wide[, (num_cols) := lapply(.SD, function(x) {
      if(any(is.na(x)) && sum(!is.na(x)) > 2) {  # Need at least 3 non-NA values
        tryCatch({
          # Suppress convergence warnings - fallback handles failures
          suppressWarnings(
            imputeTS::na_kalman(x, smooth=FALSE, model="StructTS")
          )
        }, error = function(e) {
          # Fallback to simple imputation if Kalman fails
          imputeTS::na_interpolation(x, option = "linear")
        })
      } else {
        x
      }
    }), .SDcols = num_cols]
  }

  return(DSPP_wide)
}
