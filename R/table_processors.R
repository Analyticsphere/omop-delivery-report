# ==============================================================================
# Table Processing and DQD Scoring
# ==============================================================================
# Functions for organizing tables into groups and calculating DQD scores.

#' Get table groups from configuration
#'
#' @param config Configuration list (default: default_config())
#' @return Named list of table groups
get_table_groups <- function(config = default_config()) {
  config$table_groups
}

#' Add "All Tables" group to table groups
#'
#' Creates a comprehensive group containing all unique tables.
#'
#' @param table_groups Named list of table groups
#' @return Named list with "All Tables" prepended
add_all_tables_group <- function(table_groups) {
  all_tables <- unique(unlist(table_groups, use.names = FALSE))
  c(list("All Tables" = all_tables), table_groups)
}

#' Calculate overall DQD score
#'
#' Computes percentage of DQD checks that passed.
#' A check passes if failed == 0.
#'
#' @param dqd_data Data frame with DQD results
#' @return Numeric percentage (0-100), rounded to nearest integer
calculate_overall_dqd_score <- function(dqd_data) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(NA_real_)
  }

  total_pass <- sum(dqd_data$failed == 0, na.rm = TRUE)
  total_checks <- nrow(dqd_data)

  if (total_checks == 0) {
    return(NA_real_)
  }

  score <- (total_pass / total_checks) * 100
  return(round(score, 0))
}

#' Calculate DQD score for specific tables
#'
#' @param dqd_data Data frame with DQD results
#' @param tables Character vector of table names
#' @return Numeric percentage (0-100), rounded to nearest integer, or NA if no checks
calculate_table_group_dqd_score <- function(dqd_data, tables) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(NA_real_)
  }

  # Convert table names to uppercase for matching (DQD uses uppercase)
  tables_upper <- toupper(tables)

  # Filter to checks for these tables
  table_checks <- dqd_data %>%
    dplyr::filter(toupper(cdmTableName) %in% tables_upper)

  if (nrow(table_checks) == 0) {
    return(NA_real_)
  }

  # Calculate pass rate
  total_pass <- sum(table_checks$failed == 0, na.rm = TRUE)
  total_checks <- nrow(table_checks)

  score <- (total_pass / total_checks) * 100
  return(round(score, 0))
}

#' Calculate DQD score for a single table
#'
#' @param dqd_data Data frame with DQD results
#' @param table_name Character table name
#' @return Numeric percentage (0-100), or NA if no checks
calculate_table_dqd_score <- function(dqd_data, table_name) {
  calculate_table_group_dqd_score(dqd_data, c(table_name))
}

#' Calculate DQD scores for all table groups
#'
#' @param dqd_data Data frame with DQD results
#' @param table_groups Named list of table groups
#' @return Named list of DQD scores (one per group)
calculate_all_group_dqd_scores <- function(dqd_data, table_groups) {
  if (is.null(dqd_data)) {
    # Return NA for all groups
    return(lapply(table_groups, function(x) NA_real_))
  }

  lapply(table_groups, function(tables) {
    calculate_table_group_dqd_score(dqd_data, tables)
  })
}

#' Calculate DQD scores for individual tables
#'
#' @param dqd_data Data frame with DQD results
#' @param table_names Character vector of table names
#' @return Named list of DQD scores (one per table)
calculate_all_table_dqd_scores <- function(dqd_data, table_names) {
  if (is.null(dqd_data)) {
    # Return NA for all tables
    scores <- rep(NA_real_, length(table_names))
    names(scores) <- table_names
    return(as.list(scores))
  }

  scores <- lapply(table_names, function(table) {
    calculate_table_dqd_score(dqd_data, table)
  })
  names(scores) <- table_names
  scores
}

#' Create DQD grid summary
#'
#' Aggregates DQD checks by category and context (Verification/Validation).
#'
#' @param dqd_data Data frame with DQD results
#' @return Data frame with grid structure
create_dqd_grid <- function(dqd_data) {
  if (is.null(dqd_data) || nrow(dqd_data) == 0) {
    return(tidyr::tibble(
      category = character(),
      context = character(),
      Pass = integer(),
      Fail = integer(),
      Total = integer(),
      `% Pass` = numeric()
    ))
  }

  # Extract category and context from checkName
  dqd_data <- dqd_data %>%
    dplyr::mutate(
      category = dplyr::case_when(
        grepl("plausible", checkName, ignore.case = TRUE) ~ "Plausibility",
        grepl("is", checkName, ignore.case = TRUE) ~ "Conformance",
        grepl("measure", checkName, ignore.case = TRUE) ~ "Completeness",
        TRUE ~ "Other"
      ),
      context = dplyr::case_when(
        grepl("Verification", context, ignore.case = TRUE) ~ "Verification",
        grepl("Validation", context, ignore.case = TRUE) ~ "Validation",
        TRUE ~ "Unknown"
      )
    )

  # Aggregate by category and context
  grid <- dqd_data %>%
    dplyr::group_by(category, context) %>%
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),
      Fail = sum(failed > 0, na.rm = TRUE),
      Total = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `% Pass` = round((Pass / Total) * 100, 0)
    )

  # Add totals row for each category
  category_totals <- grid %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      Pass = sum(Pass),
      Fail = sum(Fail),
      Total = sum(Total),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      context = "Total",
      `% Pass` = round((Pass / Total) * 100, 0)
    ) %>%
    dplyr::select(category, context, Pass, Fail, Total, `% Pass`)

  # Combine and sort
  grid <- dplyr::bind_rows(grid, category_totals) %>%
    dplyr::arrange(category, context)

  return(grid)
}

#' Create empty DQD scores structure
#'
#' Used when DQD data is unavailable.
#'
#' @return List with NA overall score and empty grid
create_empty_dqd_scores <- function() {
  list(
    overall = NA_real_,
    grid = create_dqd_grid(NULL)
  )
}
