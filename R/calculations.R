# ==============================================================================
# Calculations
# ==============================================================================
# Pure calculation functions with no side effects.
# Every function here should be easily testable in isolation.
# These perform mathematical operations but do not format or display data.

# ==============================================================================
# Constants
# ==============================================================================

# Tables that participate in vocabulary harmonization (clinical data only)
HARMONIZED_TABLES <- c(
  "visit_occurrence",
  "condition_occurrence",
  "drug_exposure",
  "procedure_occurrence",
  "device_exposure",
  "measurement",
  "observation",
  "note",
  "specimen"
)

# ==============================================================================
# Table Classification
# ==============================================================================

#' Check if a table participates in vocabulary harmonization
#'
#' Clinical data tables undergo vocabulary harmonization during processing,
#' while reference and derived tables do not.
#'
#' @param table_name Character table name
#' @return Logical TRUE if table is harmonized
#' @export
#' @examples
#' is_harmonized_table("drug_exposure")  # TRUE
#' is_harmonized_table("person")         # FALSE
is_harmonized_table <- function(table_name) {
  table_name %in% HARMONIZED_TABLES
}

# ==============================================================================
# Core Calculations
# ==============================================================================

#' Calculate vocabulary harmonization impact
#'
#' Determines the net row change due to vocabulary harmonization.
#' Formula: (same_table_result_rows - valid_rows) + transitions_in
#'
#' - same_table_result_rows: Rows remaining after same-table vocab mappings (includes 1:N expansion)
#' - valid_rows: Valid input rows (baseline, excludes quality issues)
#' - transitions_in: Rows received from other tables via cross-table mappings
#'
#' @param same_table_rows Integer rows after same-table mappings
#' @param valid_rows Integer valid input rows
#' @param transitions_in Integer rows received from other tables
#' @return Integer harmonization impact (negative = rows lost, positive = rows gained)
#' @export
#' @examples
#' calculate_harmonization(95, 100, 0)    # -5 (net loss)
#' calculate_harmonization(105, 100, 10)  # 15 (net gain)
#' calculate_harmonization(100, 100, 0)   # 0 (balanced)
calculate_harmonization <- function(same_table_rows, valid_rows, transitions_in) {
  (same_table_rows - valid_rows) + transitions_in
}

#' Calculate quality issues count
#'
#' Sums invalid rows and rows with missing person_id.
#'
#' @param invalid_rows Integer count of invalid rows
#' @param missing_rows Integer count of rows with missing person_id
#' @return Integer total quality issues
#' @export
calculate_quality_issues <- function(invalid_rows, missing_rows) {
  invalid_rows + missing_rows
}

#' Calculate expected final row count
#'
#' Determines expected final row count after quality filtering and harmonization.
#' Formula: initial_rows - quality_issues + harmonization
#' For non-harmonized tables, harmonization is 0.
#'
#' @param initial_rows Integer initial row count
#' @param quality_issues Integer quality issues to subtract
#' @param harmonization Integer harmonization impact
#' @return Integer expected final rows
#' @export
calculate_expected_final_rows <- function(initial_rows, quality_issues, harmonization) {
  initial_rows - quality_issues + harmonization
}

#' Calculate percentage with safe division
#'
#' Safely calculates percentage, returning 0 if denominator is 0 or NA.
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @return Numeric percentage (0-100), returns 0 if denominator is 0
#' @export
calculate_percentage <- function(numerator, denominator) {
  if (denominator == 0 || is.na(denominator) || is.na(numerator)) return(0)
  (numerator / denominator) * 100
}

#' Calculate row-per-patient metric
#'
#' Computes average rows per patient for a given table.
#'
#' @param row_count Integer total rows
#' @param patient_count Integer total patients
#' @return Numeric rows per patient (2 decimal places), returns 0 if no patients
#' @export
calculate_row_per_patient <- function(row_count, patient_count) {
  if (patient_count == 0 || is.na(patient_count) || is.na(row_count)) return(0)
  round(row_count / patient_count, 2)
}

#' Calculate total number of participants
#'
#' Sums all rows in person table (valid + invalid) to get participant count.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @return Integer participant count
#' @export
calculate_num_participants <- function(metrics) {
  valid <- get_table_count(metrics$valid_row_counts, "person")
  invalid <- get_table_count(metrics$invalid_row_counts, "person")
  valid + invalid
}

#' Calculate total rows removed due to missing person_id
#'
#' Sums rows removed across all tables due to missing person_id values.
#'
#' @param metrics List of metric data frames from parse_delivery_metrics()
#' @return Integer total rows removed
#' @export
calculate_total_rows_removed <- function(metrics) {
  if (is.null(metrics$missing_person_id) || nrow(metrics$missing_person_id) == 0) {
    return(0)
  }

  total <- sum(metrics$missing_person_id$count, na.rm = TRUE)
  return(total)
}

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Extract count for a table from metrics
#'
#' Helper to safely extract a count value from a metric data frame.
#'
#' @param metric_df Data frame with table_name and count columns
#' @param table_name Character table name to extract
#' @return Integer count (0 if not found)
get_table_count <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  value <- metric_df |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(count)

  ifelse(length(value) > 0, value[1], 0)
}

#' Extract sum of counts for a table from metrics
#'
#' For metrics that may have multiple rows per table (e.g., default_dates by column).
#'
#' @param metric_df Data frame with table_name and count columns
#' @param table_name Character table name to extract
#' @return Integer sum of counts (0 if not found)
get_table_count_sum <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  total <- metric_df |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)

  ifelse(length(total) > 0, total[1], 0)
}
