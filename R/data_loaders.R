# ==============================================================================
# Data Loading and Validation
# ==============================================================================
# Handles loading delivery report and DQD data from CSV files.
# Validates schema and data quality.

library(logger)

#' Load delivery report data from CSV
#'
#' Reads delivery_report.csv from local filesystem or GCS.
#' Returns NULL if file is missing (with warning).
#'
#' @param path Character path to delivery_report.csv (local or gs:// URI)
#' @return Data frame with delivery report data, or NULL if file not found
#' @export
load_delivery_data <- function(path) {
  logger::log_info("Loading delivery report from: {path}")

  # Use GCS-aware read function
  data <- read_csv(path)

  if (is.null(data)) {
    logger::log_warn("Delivery report not found")
    return(NULL)
  }

  logger::log_info("Loaded delivery report: {nrow(data)} rows")

  # Validate schema
  validate_delivery_schema(data)

  return(data)
}

#' Load DQD results data from CSV
#'
#' Reads dqd_results.csv from local filesystem or GCS.
#' Returns NULL if file is missing (with warning).
#'
#' @param path Character path to dqd_results.csv (local or gs:// URI)
#' @return Data frame with DQD results, or NULL if file not found
#' @export
load_dqd_data <- function(path) {
  logger::log_info("Loading DQD results from: {path}")

  # Use GCS-aware read function
  data <- read_csv(path)

  if (is.null(data)) {
    logger::log_warn("DQD results not found")
    return(NULL)
  }

  logger::log_info("Loaded DQD results: {nrow(data)} rows")

  # Validate schema
  validate_dqd_schema(data)

  return(data)
}

#' Validate delivery report schema
#'
#' Checks that all required columns are present.
#' The delivery report uses metadata format with name/value columns.
#'
#' @param data Data frame to validate
#' @return Logical TRUE if valid (or stops with error)
validate_delivery_schema <- function(data) {
  required_columns <- c(
    "name",
    "value_as_string",
    "value_as_number"
  )

  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    stop("Delivery report missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  return(TRUE)
}

#' Validate DQD results schema
#'
#' Checks that all required columns are present.
#' DQD uses standard OHDSI Data Quality Dashboard output format.
#'
#' @param data Data frame to validate
#' @return Logical TRUE if valid (or stops with error)
validate_dqd_schema <- function(data) {
  required_columns <- c(
    "checkName",
    "cdmTableName",
    "failed",
    "context"
  )

  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    stop("DQD results missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  return(TRUE)
}

#' Check data availability flags
#'
#' Determines which data sources are available.
#'
#' @param delivery_data Delivery report data frame or NULL
#' @param dqd_data DQD results data frame or NULL
#' @return List with has_delivery_data and has_dqd_data flags
check_data_availability <- function(delivery_data, dqd_data) {
  list(
    has_delivery_data = !is.null(delivery_data),
    has_dqd_data = !is.null(dqd_data)
  )
}
