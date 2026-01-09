# ==============================================================================
# Metric Calculation Functions
# ==============================================================================
# Pure calculation functions with no side effects.
# Every function here should be easily testable in isolation.

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

#' Check if a table participates in vocabulary harmonization
#'
#' @param table_name Character table name
#' @return Logical TRUE if table is harmonized
is_harmonized_table <- function(table_name) {
  table_name %in% HARMONIZED_TABLES
}

#' Calculate vocabulary harmonization impact
#'
#' Formula: (same_table_result_rows - valid_rows) + transitions_in
#' - same_table_result_rows: Rows remaining after same-table vocab mappings (includes 1:N expansion)
#' - valid_rows: Valid input rows (baseline, excludes quality issues)
#' - transitions_in: Rows received from other tables via cross-table mappings
#'
#' @param same_table_rows Integer rows after same-table mappings
#' @param valid_rows Integer valid input rows
#' @param transitions_in Integer rows received from other tables
#' @return Integer harmonization impact (negative = rows lost, positive = rows gained)
#' @examples
#' calculate_harmonization(95, 100, 0)    # -5 (net loss)
#' calculate_harmonization(105, 100, 10)  # 15 (net gain)
#' calculate_harmonization(100, 100, 0)   # 0 (balanced)
calculate_harmonization <- function(same_table_rows, valid_rows, transitions_in) {
  (same_table_rows - valid_rows) + transitions_in
}

#' Calculate quality issues count
#'
#' @param invalid_rows Integer count of invalid rows
#' @param missing_rows Integer count of rows with missing person_id
#' @return Integer total quality issues
calculate_quality_issues <- function(invalid_rows, missing_rows) {
  invalid_rows + missing_rows
}

#' Calculate expected final row count
#'
#' Formula: initial_rows - quality_issues + harmonization
#' For non-harmonized tables, harmonization is 0.
#'
#' @param initial_rows Integer initial row count
#' @param quality_issues Integer quality issues to subtract
#' @param harmonization Integer harmonization impact
#' @return Integer expected final rows
calculate_expected_final_rows <- function(initial_rows, quality_issues, harmonization) {
  initial_rows - quality_issues + harmonization
}

#' Calculate percentage with safe division
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @return Numeric percentage (0-100), returns 0 if denominator is 0
calculate_percentage <- function(numerator, denominator) {
  if (denominator == 0 || is.na(denominator) || is.na(numerator)) return(0)
  (numerator / denominator) * 100
}

#' Calculate row-per-patient metric
#'
#' @param row_count Integer total rows
#' @param patient_count Integer total patients
#' @return Numeric rows per patient (2 decimal places), returns 0 if no patients
calculate_row_per_patient <- function(row_count, patient_count) {
  if (patient_count == 0 || is.na(patient_count) || is.na(row_count)) return(0)
  round(row_count / patient_count, 2)
}

#' Determine DQD score class for styling
#'
#' @param score Numeric DQD score (0-100) or NA
#' @return Character CSS class name ("good", "fair", "poor", or "neutral")
get_dqd_score_class <- function(score) {
  if (is.na(score)) {
    return("neutral")
  } else if (score >= 95) {
    return("good")
  } else if (score >= 85) {
    return("fair")
  } else {
    return("poor")
  }
}

#' Format numeric value for display
#'
#' @param value Numeric value
#' @param big_mark Character to use as thousands separator (default: ",")
#' @return Character formatted number
format_number <- function(value, big_mark = ",") {
  if (is.na(value) || is.null(value)) return("0")
  format(value, big.mark = big_mark, scientific = FALSE)
}

#' Calculate percentage and format for display
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @param decimal_places Integer number of decimal places (default: 1)
#' @return Character formatted percentage (e.g., "25.5")
format_percentage <- function(numerator, denominator, decimal_places = 1) {
  pct <- calculate_percentage(numerator, denominator)
  format(round(pct, decimal_places), nsmall = decimal_places)
}

#' Determine status icon text based on delivery status
#'
#' @param delivered Logical whether table was delivered
#' @return List with icon text and CSS class
get_status_badge <- function(delivered) {
  if (delivered) {
    list(text = "Delivered", class = "delivered")
  } else {
    list(text = "Not Delivered", class = "not-delivered")
  }
}

#' Determine metric card class based on value and threshold
#'
#' @param value Numeric value to evaluate
#' @param zero_is_good Logical whether zero is good (TRUE) or bad (FALSE)
#' @return Character CSS class ("success", "warning", or "neutral")
get_metric_class <- function(value, zero_is_good = TRUE) {
  if (is.na(value)) return("neutral")

  if (zero_is_good) {
    if (value == 0) return("success")
    return("warning")
  } else {
    if (value > 0) return("success")
    return("neutral")
  }
}

#' Format harmonization value for display with sign
#'
#' @param harmonization Integer harmonization value
#' @param is_harmonized_table Logical whether table participates in harmonization
#' @return List with display text and CSS class
format_harmonization_display <- function(harmonization, is_harmonized_table) {
  if (!is_harmonized_table) {
    return(list(text = "--", class = "harmonization-neutral"))
  }

  if (harmonization > 0) {
    return(list(
      text = paste0("+", format_number(harmonization)),
      class = "harmonization-positive"
    ))
  } else if (harmonization < 0) {
    return(list(
      text = format_number(harmonization),
      class = "harmonization-negative"
    ))
  } else {
    return(list(
      text = "0",
      class = "harmonization-neutral"
    ))
  }
}

#' Format quality issues for display with sign
#'
#' @param quality_issues Integer quality issues count
#' @return List with display text and CSS class
format_quality_issues_display <- function(quality_issues) {
  if (quality_issues > 0) {
    return(list(
      text = paste0("-", format_number(quality_issues)),
      class = "harmonization-negative"
    ))
  } else {
    return(list(
      text = format_number(quality_issues),
      class = "harmonization-neutral"
    ))
  }
}
