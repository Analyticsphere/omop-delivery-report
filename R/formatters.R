# ==============================================================================
# Formatters
# ==============================================================================
# Display formatting functions for presenting data in the report.
# These functions take calculated values and format them for human consumption.
# They determine styling classes and apply formatting rules.

#' Format numeric value for display
#'
#' Formats numbers with thousands separators for readability.
#'
#' @param value Numeric value
#' @param big_mark Character to use as thousands separator (default: ",")
#' @return Character formatted number
#' @export
#' @examples
#' format_number(1000)      # "1,000"
#' format_number(1234567)   # "1,234,567"
format_number <- function(value, big_mark = ",") {
  if (is.na(value) || is.null(value)) return("0")
  format(value, big.mark = big_mark, scientific = FALSE)
}

#' Calculate percentage and format for display
#'
#' Combines calculation and formatting in one step for convenience.
#'
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @param decimal_places Integer number of decimal places (default: 1)
#' @return Character formatted percentage (e.g., "25.5")
#' @export
format_percentage <- function(numerator, denominator, decimal_places = 1) {
  pct <- calculate_percentage(numerator, denominator)
  format(round(pct, decimal_places), nsmall = decimal_places)
}

#' Determine DQD score class for styling
#'
#' Maps DQD scores to CSS classes for color-coded display.
#' - >= 95%: "good" (green)
#' - >= 85%: "fair" (yellow)
#' - < 85%: "poor" (red)
#' - NA: "neutral" (gray)
#'
#' @param score Numeric DQD score (0-100) or NA
#' @return Character CSS class name ("good", "fair", "poor", or "neutral")
#' @export
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

#' Determine status badge based on delivery status
#'
#' @param delivered Logical whether table was delivered
#' @return List with status text and CSS class
#' @export
get_status_badge <- function(delivered) {
  if (delivered) {
    list(text = "Delivered", class = "delivered")
  } else {
    list(text = "Not Delivered", class = "not-delivered")
  }
}

#' Determine metric card class based on value and threshold
#'
#' Determines styling based on whether zero is considered good or bad.
#'
#' @param value Numeric value to evaluate
#' @param zero_is_good Logical whether zero is good (TRUE) or bad (FALSE)
#' @return Character CSS class ("success", "warning", or "neutral")
#' @export
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
#' Formats harmonization values with appropriate signs and styling.
#' - Positive values: "+X" (green)
#' - Negative values: "-X" (red)
#' - Zero: "0" (neutral)
#' - Non-harmonized tables: "--" (neutral)
#'
#' @param harmonization Integer harmonization value
#' @param is_harmonized_table Logical whether table participates in harmonization
#' @return List with display text and CSS class
#' @export
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
#' Formats quality issue counts with minus sign if issues exist.
#'
#' @param quality_issues Integer quality issues count
#' @return List with display text and CSS class
#' @export
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
