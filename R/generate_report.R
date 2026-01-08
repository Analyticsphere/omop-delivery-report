# ==============================================================================
# OMOP Delivery Report Generator
# ==============================================================================
# Description: Generates a comprehensive HTML report from OMOP delivery artifacts
# Author: Claude Code
# Date: 2025-12-15
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------------------------

# Load required packages
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(networkD3)
library(rmarkdown)
library(RColorBrewer)

# Load GCS utilities for cloud storage support
if (file.exists("R/gcs_utils.R")) {
  source("R/gcs_utils.R")
} else if (file.exists("gcs_utils.R")) {
  source("gcs_utils.R")
}

# ------------------------------------------------------------------------------
# 2. DATA LOADING FUNCTIONS
# ------------------------------------------------------------------------------

#' Load and validate delivery report data
#'
#' Reads delivery_report.csv from local filesystem or GCS.
#' Returns NULL if file is missing (with warning) to allow graceful degradation.
#' Supports both local paths and GCS URIs (gs://bucket/path).
#'
#' @param file_path Character. Path to delivery_report.csv (local or gs:// URI)
#' @return Tibble with columns: name, value_as_string, value_as_number, or NULL if file missing
load_delivery_report <- function(file_path) {
  # Use smart reader that handles both local and GCS paths
  data <- read_csv(file_path)

  if (is.null(data)) {
    warning("Delivery report file not found: ", file_path, "\nReport will be generated without delivery metrics.")
    return(NULL)
  }

  # Validate required columns
  required_cols <- c("name", "value_as_string", "value_as_number")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  message("Loaded delivery report: ", nrow(data), " rows")
  return(dplyr::as_tibble(data))
}

#' Load and validate DQD results data
#'
#' Reads Data Quality Dashboard results CSV from local filesystem or GCS.
#' Returns NULL if file is missing (with warning) to allow graceful degradation.
#' Supports both local paths and GCS URIs (gs://bucket/path).
#'
#' @param file_path Character. Path to dqd_results.csv (local or gs:// URI)
#' @return Tibble with DQD check results including checkId, failed, passed, category, cdmTableName, or NULL if file missing
load_dqd_results <- function(file_path) {
  # Use smart reader that handles both local and GCS paths
  data <- read_csv(file_path)

  if (is.null(data)) {
    warning("DQD results file not found: ", file_path, "\nReport will be generated without DQD metrics.")
    return(NULL)
  }

  # Validate required columns
  required_cols <- c("checkId", "failed", "passed", "category", "cdmTableName")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  message("Loaded DQD results: ", nrow(data), " rows")
  return(dplyr::as_tibble(data))
}

# ------------------------------------------------------------------------------
# 2B. EMPTY DATA STRUCTURE HELPERS
# ------------------------------------------------------------------------------

#' Create empty metrics structure
#'
#' Returns an empty metrics list with same structure as parse_delivery_metrics()
#' but with empty tibbles. Used when delivery data is unavailable.
#'
#' @return List of empty metric tibbles
create_empty_metrics <- function() {
  list(
    valid_columns = tibble(table_name = character(), column_name = character()),
    invalid_columns = tibble(table_name = character(), column_name = character()),
    valid_tables = tibble(table_name = character()),
    invalid_tables = tibble(table_name = character()),
    valid_row_counts = tibble(table_name = character(), count = numeric()),
    invalid_row_counts = tibble(table_name = character(), count = numeric()),
    final_row_counts = tibble(table_name = character(), count = numeric()),
    type_concepts = tibble(table_name = character(), type_concept = character(), count = numeric()),
    type_concepts_grouped = tibble(table_name = character(), type_concept = character(), count = numeric(), type_group = character()),
    source_vocabularies = tibble(table_name = character(), column_name = character(), vocabulary = character(), count = numeric()),
    target_vocabularies = tibble(table_name = character(), column_name = character(), vocabulary = character(), count = numeric()),
    table_transitions = tibble(source_table = character(), target_table = character(), count = numeric()),
    harmonization_statuses = tibble(table_name = character(), status = character(), count = numeric()),
    same_table_mappings = tibble(table_name = character(), mapping = character(), source_multiplier = numeric(), target_multiplier = numeric(), total_rows = numeric()),
    rows_added_by_table = tibble(table_name = character(), rows_added = numeric()),
    row_dispositions = tibble(table_name = character(), disposition = character(), count = numeric()),
    missing_person_id = tibble(table_name = character(), count = numeric()),
    referential_integrity_violations = tibble(table_name = character(), count = numeric()),
    default_date_values = tibble(table_name = character(), column_name = character(), count = numeric()),
    invalid_concepts = tibble(table_name = character(), column_name = character(), count = numeric()),
    missing_columns = tibble(table_name = character(), column_name = character()),
    time_series = tibble(table_name = character(), year = integer(), count = numeric()),
    metadata = list(
      site = "Unknown",
      delivery_date = "Unknown",
      processing_date = "Unknown",
      delivered_cdm_version = "Unknown",
      delivered_vocab_version = "Unknown",
      standardized_cdm_version = "Unknown",
      standardized_vocab_version = "Unknown",
      pipeline_version = "Unknown",
      file_format = "Unknown"
    ),
    missing_person_id_count = 0
  )
}

#' Create empty DQD scores structure
#'
#' Returns empty DQD score structures. Used when DQD data is unavailable.
#'
#' @return List with overall score (NA) and empty grid
create_empty_dqd_scores <- function() {
  list(
    overall = NA_real_,
    grid = tibble(
      category = character(),
      context = character(),
      Pass = numeric(),
      Fail = numeric(),
      Total = numeric(),
      `% Pass` = numeric()
    )
  )
}

# ------------------------------------------------------------------------------
# 3. METRIC PARSING FUNCTIONS
# ------------------------------------------------------------------------------

#' Parse delivery report metrics into structured format
#'
#' Extracts and organizes metrics from delivery_report.csv into categorized tibbles.
#' Parses table counts, type concepts, vocabularies, harmonization data, and metadata.
#'
#' @param delivery_data Tibble from load_delivery_report()
#' @return List containing parsed metrics: valid_columns, invalid_columns, valid_tables,
#'   valid_row_counts, invalid_row_counts, final_row_counts, type_concepts,
#'   source_vocabularies, target_vocabularies, harmonization_statuses, metadata, etc.
parse_delivery_metrics <- function(delivery_data) {

  metrics <- list()

  # Parse valid column names
  metrics$valid_columns <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Valid column name:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid column name: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Valid column name: (\\w+)\\.(\\w+)")[, 3]
    ) %>%
    dplyr::select(table_name, column_name)

  # Parse invalid column names
  metrics$invalid_columns <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Invalid column name:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid column name: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Invalid column name: (\\w+)\\.(\\w+)")[, 3]
    ) %>%
    dplyr::select(table_name, column_name)

  # Parse valid table names
  metrics$valid_tables <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Valid table name:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid table name: (\\w+)")[, 2]
    ) %>%
    dplyr::select(table_name) %>%
    dplyr::distinct()

  # Parse invalid table names
  metrics$invalid_tables <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Invalid table name:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid table name: (\\w+)")[, 2]
    ) %>%
    dplyr::select(table_name) %>%
    dplyr::distinct()

  # Parse valid row counts
  metrics$valid_row_counts <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Valid row count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid row count: (\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, count)

  # Parse invalid row counts
  metrics$invalid_row_counts <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Invalid row count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid row count: (\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, count)

  # Parse final row counts
  metrics$final_row_counts <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Final row count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Final row count: (\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, count)

  # Parse type concept breakdowns
  metrics$type_concepts <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Type concept breakdown:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Type concept breakdown: (\\w+)")[, 2],
      type_concept = value_as_string,
      count = value_as_number
    ) %>%
    dplyr::select(table_name, type_concept, count)

  # Parse source vocabulary breakdowns
  metrics$source_vocabularies <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Source vocabulary breakdown:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Source vocabulary breakdown: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Source vocabulary breakdown: (\\w+)\\.(\\w+)")[, 3],
      vocabulary = value_as_string,
      count = value_as_number
    ) %>%
    dplyr::select(table_name, column_name, vocabulary, count)

  # Parse target vocabulary breakdowns
  metrics$target_vocabularies <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Target vocabulary breakdown:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Target vocabulary breakdown: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Target vocabulary breakdown: (\\w+)\\.(\\w+)")[, 3],
      vocabulary = value_as_string,
      count = value_as_number
    ) %>%
    dplyr::select(table_name, column_name, vocabulary, count)

  # Parse vocab harmonization table transitions
  metrics$table_transitions <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization table transition:")) %>%
    dplyr::mutate(
      source_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 2],
      target_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 3],
      count = value_as_number
    ) %>%
    dplyr::select(source_table, target_table, count)

  # Parse vocab harmonization statuses (exclude "domain check" per requirements)
  metrics$harmonization_statuses <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization status:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization status: (\\w+) - ")[, 2],
      status = stringr::str_match(name, "Vocab harmonization status: \\w+ - (.+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::filter(!stringr::str_detect(tolower(status), "domain check")) %>%  # Exclude domain check
    dplyr::select(table_name, status, count)

  # Parse vocab harmonization same-table mappings
  # The artifact total_rows represents RESULT ROW COUNT in the harmonized data
  # For display purposes, we calculate rows_added to show in the 1:N breakdown table
  metrics$same_table_mappings <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization same-table mapping:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization same-table mapping: (\\w+) - ")[, 2],
      mapping = stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 1],
      source_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 2]),
      target_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 3]),
      total_rows = value_as_number,
      # For display: calculate how many rows were added by this specific mapping ratio
      rows_added = total_rows * ((target_multiplier - 1) / target_multiplier)
    ) %>%
    dplyr::select(table_name, mapping, source_multiplier, target_multiplier, total_rows, rows_added)

  # Parse vocab harmonization row dispositions (kept for informational purposes)
  metrics$row_dispositions <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization row disposition:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization row disposition: (\\w+) - ")[, 2],
      disposition = stringr::str_match(name, "Vocab harmonization row disposition: \\w+ - (.+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, disposition, count)

  # Parse missing person_id metrics
  metrics$missing_person_id <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "Number of rows removed due to missing person_id values:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Number of rows removed due to missing person_id values: (\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, count)

  # Parse person_id referential integrity violations
  metrics$referential_integrity_violations <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Person_id referential integrity violation count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Person_id referential integrity violation count: (\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, count)

  # Parse default date value counts
  metrics$default_date_values <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Date/datetime default value count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Date/datetime default value count: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Date/datetime default value count: \\w+\\.(\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::select(table_name, column_name, count)

  # Parse invalid concept_id counts
  # Only return invalid concept count for NON-source concept ID columns
  metrics$invalid_concepts <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Invalid concept_id count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid concept_id count: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Invalid concept_id count: \\w+\\.(\\w+)")[, 2],
      count = value_as_number
    ) %>%
    dplyr::filter(!stringr::str_detect(column_name, "_source_concept_id$")) %>%
    dplyr::select(table_name, column_name, count)

  # Parse missing columns
  metrics$missing_columns <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Missing column:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Missing column: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Missing column: \\w+\\.(\\w+)")[, 2]
    ) %>%
    dplyr::select(table_name, column_name)

  # Parse technical metadata
  # Convert delivery_date to YYYY-MM-DD (handles both M/D/YY and YYYY-MM-DD formats)
  delivery_date_raw <- delivery_data %>% dplyr::filter(name == "Delivery date") %>% dplyr::pull(value_as_string)
  delivery_date_formatted <- tryCatch({
    # Try parsing as M/D/YY format first
    date_obj <- as.Date(delivery_date_raw, format = "%m/%d/%y")
    if (is.na(date_obj)) {
      # Try YYYY-MM-DD format
      date_obj <- as.Date(delivery_date_raw, format = "%Y-%m-%d")
    }
    # Format as YYYY-MM-DD
    format(date_obj, "%Y-%m-%d")
  }, error = function(e) {
    # If parsing fails, return original
    delivery_date_raw
  })

  # Convert processing_date to YYYY-MM-DD (handles both MM/DD/YY and YYYY-MM-DD formats)
  processing_date_raw <- delivery_data %>% dplyr::filter(name == "Delivery processing date") %>% dplyr::pull(value_as_string)
  processing_date_formatted <- tryCatch({
    # Try parsing as MM/DD/YY format first
    date_obj <- as.Date(processing_date_raw, format = "%m/%d/%y")
    if (is.na(date_obj)) {
      # Try YYYY-MM-DD format
      date_obj <- as.Date(processing_date_raw, format = "%Y-%m-%d")
    }
    # Format as YYYY-MM-DD
    format(date_obj, "%Y-%m-%d")
  }, error = function(e) {
    # If parsing fails, return original
    processing_date_raw
  })

  metrics$metadata <- list(
    site = delivery_data %>% dplyr::filter(name == "Site") %>% dplyr::pull(value_as_string),
    delivery_date = delivery_date_formatted,
    processing_date = processing_date_formatted,
    delivered_cdm_version = delivery_data %>% dplyr::filter(name == "Delivered CDM version") %>% dplyr::pull(value_as_string),
    delivered_vocab_version = delivery_data %>% dplyr::filter(name == "Delivered vocabulary version") %>% dplyr::pull(value_as_string),
    standardized_cdm_version = delivery_data %>% dplyr::filter(name == "Standardized to CDM version") %>% dplyr::pull(value_as_string),
    standardized_vocab_version = delivery_data %>% dplyr::filter(name == "Standardized to vocabulary version") %>% dplyr::pull(value_as_string),
    pipeline_version = delivery_data %>% dplyr::filter(name == "Pipeline file processor version") %>% dplyr::pull(value_as_string),
    file_format = delivery_data %>% dplyr::filter(name == "File delivery format") %>% dplyr::pull(value_as_string)
  )

  # Get number of persons with missing person_id
  metrics$missing_person_id_count <- delivery_data %>%
    dplyr::filter(name == "Number of persons with missing person_id") %>%
    dplyr::pull(value_as_number)

  if (length(metrics$missing_person_id_count) == 0) {
    metrics$missing_person_id_count <- 0
  }

  # Parse time series row counts
  metrics$time_series <- delivery_data %>%
    dplyr::filter(stringr::str_detect(name, "^Time series row count:")) %>%
    dplyr::mutate(
      table_name = stringr::str_match(name, "Time series row count: (\\w+)\\.(\\d+)")[, 2],
      year = as.integer(stringr::str_match(name, "Time series row count: (\\w+)\\.(\\d+)")[, 3]),
      count = value_as_number
    ) %>%
    dplyr::select(table_name, year, count) %>%
    dplyr::arrange(table_name, year)

  return(metrics)
}

# ------------------------------------------------------------------------------
# 4. TYPE CONCEPT GROUPING
# ------------------------------------------------------------------------------

#' Apply type concept grouping rules
#'
#' Categorizes OMOP type concepts into broader groups (EHR, Claims, Disease registry,
#' Patient reported, Unlabeled, Other) based on naming patterns.
#'
#' @param type_concepts Tibble with columns: table_name, type_concept, count
#' @return Tibble with additional column: type_group
group_type_concepts <- function(type_concepts) {

  type_concepts %>%
    dplyr::mutate(
      type_group = dplyr::case_when(
        # Unlabeled group (NULL, blank, "0", or "No matching concept")
        is.na(type_concept) |
          type_concept == "" |
          type_concept == "0" |
          tolower(type_concept) == "no matching concept" ~ "Unlabeled",
        # EHR group (case-sensitive)
        stringr::str_detect(type_concept, "EHR") ~ "EHR",
        # Claims group (case-insensitive)
        stringr::str_detect(tolower(type_concept), "claim") ~ "Claims",
        # Claims group (payer records)
        stringr::str_detect(tolower(type_concept), "payer system record") ~ "Claims",
        # Disease registry group
        type_concept %in% c("Registry", "Tumor Registry") ~ "Disease registry",
        # Patient reported group
        type_concept %in% c("Patient self-report", "Patient self-tested",
                            "Patient filled survey", "Survey",
                            "Patient Self-Reported Medication") ~ "Patient reported",
        # Ungrouped
        TRUE ~ "Other"
      )
    )
}

#' Ensure all type concept groups are present and ordered
#'
#' Takes aggregated type concept data and ensures all groups from the canonical
#' order are present (with 0 counts if missing), then orders them consistently.
#'
#' @param type_concept_data Tibble with columns: type_group, count (and optionally others)
#' @param config Configuration list from merge_config()
#' @return Tibble with all groups present and properly ordered
ensure_all_type_groups <- function(type_concept_data, config = default_config()) {
  all_groups <- get_type_concept_group_order(config)

  # Create a complete template with all groups
  complete_groups <- tibble::tibble(
    type_group = all_groups,
    count = 0
  )

  # Merge with actual data, keeping all groups
  result <- complete_groups %>%
    dplyr::left_join(
      type_concept_data %>% dplyr::select(type_group, count),
      by = "type_group",
      suffix = c("_template", "_actual")
    ) %>%
    dplyr::mutate(
      count = dplyr::coalesce(count_actual, count_template)
    ) %>%
    dplyr::select(-count_template, -count_actual)

  # If original data had additional columns, merge them back
  extra_cols <- setdiff(names(type_concept_data), c("type_group", "count"))
  if (length(extra_cols) > 0) {
    result <- result %>%
      dplyr::left_join(
        type_concept_data %>% dplyr::select(type_group, dplyr::all_of(extra_cols)),
        by = "type_group"
      )
  }

  # Ensure proper ordering
  result %>%
    dplyr::mutate(type_group = factor(type_group, levels = all_groups)) %>%
    dplyr::arrange(type_group) %>%
    dplyr::mutate(type_group = as.character(type_group))
}

# ------------------------------------------------------------------------------
# 5. DQD SCORE CALCULATIONS
# ------------------------------------------------------------------------------

#' Calculate overall DQD score
#'
#' Computes percentage of DQD checks that passed (failed == 0).
#'
#' @param dqd_data Tibble from load_dqd_results()
#' @return Numeric percentage (0-100), rounded to nearest integer
calculate_overall_dqd_score <- function(dqd_data) {
  # Count number of checks that passed (failed == 0) vs failed (failed > 0)
  # This matches the logic in create_dqd_grid for consistency
  total_pass <- sum(dqd_data$failed == 0, na.rm = TRUE)
  total_fail <- sum(dqd_data$failed > 0, na.rm = TRUE)

  if (total_pass + total_fail == 0) return(0)

  score <- (total_pass / (total_pass + total_fail)) * 100
  return(round(score, 0))
}

#' Calculate DQD score for specific tables
#' @param dqd_data Tibble from load_dqd_results()
#' @param tables Character vector of table names
#' @return Numeric percentage (0-100)
calculate_table_group_dqd_score <- function(dqd_data, tables) {

  # Convert table names to uppercase for matching
  tables_upper <- toupper(tables)

  filtered_data <- dqd_data %>%
    dplyr::filter(toupper(cdmTableName) %in% tables_upper)

  if (nrow(filtered_data) == 0) return(NA)

  # Count number of checks that passed (failed == 0) vs failed (failed > 0)
  total_pass <- sum(filtered_data$failed == 0, na.rm = TRUE)
  total_fail <- sum(filtered_data$failed > 0, na.rm = TRUE)

  if (total_pass + total_fail == 0) return(NA)

  score <- (total_pass / (total_pass + total_fail)) * 100
  return(round(score, 0))
}

#' Calculate DQD score for a single table
#' @param dqd_data Tibble from load_dqd_results()
#' @param table_name Character string of table name
#' @return Numeric percentage (0-100)
calculate_table_dqd_score <- function(dqd_data, table_name) {
  calculate_table_group_dqd_score(dqd_data, c(table_name))
}

#' Create DQD grid summary (like the PNG example)
#' @param dqd_data Tibble from load_dqd_results()
#' @return Tibble with grid structure
create_dqd_grid <- function(dqd_data) {

  # Calculate for each category and context
  # Count number of checks (rows), not sum of passed/failed records
  grid_data <- dqd_data %>%
    dplyr::group_by(category, context) %>%
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),  # Count checks with no failures
      Fail = sum(failed > 0, na.rm = TRUE),   # Count checks with failures
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Total = Pass + Fail,
      `% Pass` = round((Pass / Total) * 100, 0)
    )

  # Add totals for each category (across contexts)
  category_totals <- dqd_data %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),
      Fail = sum(failed > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Total = Pass + Fail,
      `% Pass` = round((Pass / Total) * 100, 0),
      context = "Total"
    )

  # Add overall totals (one for each context)
  overall_by_context <- dqd_data %>%
    dplyr::group_by(context) %>%
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),
      Fail = sum(failed > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Total = Pass + Fail,
      `% Pass` = round((Pass / Total) * 100, 0),
      category = "Total"
    )

  # Add grand total (across all contexts)
  grand_total <- dqd_data %>%
    dplyr::summarise(
      Pass = sum(failed == 0, na.rm = TRUE),
      Fail = sum(failed > 0, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      Total = Pass + Fail,
      `% Pass` = round((Pass / Total) * 100, 0),
      category = "Total",
      context = "Total"
    )

  # Combine all
  full_grid <- dplyr::bind_rows(grid_data, category_totals, overall_by_context, grand_total)

  return(full_grid)
}

# ------------------------------------------------------------------------------
# 6. TABLE GROUP DEFINITIONS
# ------------------------------------------------------------------------------

#' Get table group definitions
#' @return Named list of table groups
get_table_groups <- function() {
  # Define individual groups
  groups <- list(
    "Clinical Data" = c("person", "visit_occurrence", "visit_detail", "condition_occurrence",
                        "drug_exposure", "procedure_occurrence", "device_exposure",
                        "measurement", "observation", "death", "note", "specimen"),
    "Health System" = c("location", "care_site", "provider"),
    "Healthcare Economics" = c("payer_plan_period", "cost"),
    "Derived Data" = c("observation_period", "drug_era", "condition_era", "dose_era"),
    "Vocabulary" = c("concept", "vocabulary", "domain", "concept_class",
                     "concept_relationship", "relationship", "concept_synonym",
                     "concept_ancestor", "source_to_concept_map", "drug_strength"),
    "Metadata" = c("metadata", "cdm_source"),
    "Other" = c("note_nlp", "fact_relationship", "cohort", "cohort_definition",
                "episode", "episode_event", "attribute_definition")
  )

  # Add "All Tables" as the first group
  all_tables <- unique(unlist(groups, use.names = FALSE))
  c(list("All Tables" = all_tables), groups)
}

#' Get all expected tables
#' @return Character vector of all table names
get_all_expected_tables <- function() {
  table_groups <- get_table_groups()
  unique(unlist(table_groups, use.names = FALSE))
}

#' Identify non-delivered tables
#' @param metrics Parsed metrics from parse_delivery_metrics()
#' @return Character vector of non-delivered table names
identify_non_delivered_tables <- function(metrics) {

  # Tables with "Final row count" are expected
  expected_tables <- unique(metrics$final_row_counts$table_name)

  # Tables with "Valid table name" were delivered
  delivered_tables <- unique(metrics$valid_tables$table_name)

  # Non-delivered = expected - delivered
  non_delivered <- setdiff(expected_tables, delivered_tables)

  return(non_delivered)
}

# ------------------------------------------------------------------------------
# 7. COLOR PALETTE (COLOR-BLIND FRIENDLY)
# ------------------------------------------------------------------------------

#' Get color-blind friendly palette for type concept groups
#' @return Named vector of colors
get_type_concept_colors <- function() {
  # Using colorblind-friendly palette from viridis and RColorBrewer
  c(
    "EHR" = "#0073C2",           # Blue
    "Claims" = "#EFC000",        # Yellow
    "Disease registry" = "#A95AA1", # Purple/Magenta
    "Patient reported" = "#CD534C", # Red
    "Unlabeled" = "#868686",     # Gray
    "Other" = "#7AA6DC"          # Light blue
  )
}

#' Get extended color palette for individual type concepts
#' @param n Number of colors needed
#' @return Vector of hex colors
get_extended_palette <- function(n) {
  if (n <= 8) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set2"))
  } else {
    return(colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n))
  }
}

# ------------------------------------------------------------------------------
# 8. VISUALIZATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Create Sankey diagram for table transitions
#' @param table_transitions Tibble with source_table, target_table, count
#' @param table_filter Optional vector of tables to filter
#' @return networkD3 Sankey diagram object
create_sankey_diagram <- function(table_transitions, table_filter = NULL) {

  # Filter if needed
  if (!is.null(table_filter)) {
    table_transitions <- table_transitions %>%
      dplyr::filter(source_table %in% table_filter | target_table %in% table_filter)
  }

  if (nrow(table_transitions) == 0) {
    return(NULL)
  }

  # Remove self-transitions (table to itself)
  table_transitions <- table_transitions %>%
    dplyr::filter(source_table != target_table)

  if (nrow(table_transitions) == 0) {
    return(NULL)
  }

  # Create nodes
  nodes <- data.frame(
    name = unique(c(table_transitions$source_table, table_transitions$target_table))
  )

  # Create links
  links <- table_transitions %>%
    dplyr::mutate(
      source = match(source_table, nodes$name) - 1,
      target = match(target_table, nodes$name) - 1,
      value = count
    ) %>%
    dplyr::select(source, target, value)

  # Create Sankey diagram
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    units = "rows",
    fontSize = 12,
    nodeWidth = 30,
    sinksRight = FALSE
  )

  return(sankey)
}

#' Generate table drilldown data
#' @param table_name Name of the table
#' @param metrics Parsed metrics
#' @param dqd_data DQD results data
#' @return List with all drilldown information
generate_table_drilldown_data <- function(table_name, metrics, dqd_data) {

  drilldown <- list()
  drilldown$table_name <- table_name

  # DQD score for this table
  drilldown$dqd_score <- calculate_table_dqd_score(dqd_data, table_name)

  # Row counts
  valid_rows <- metrics$valid_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  valid_rows <- ifelse(length(valid_rows) > 0, valid_rows[1], 0)

  invalid_rows <- metrics$invalid_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  invalid_rows <- ifelse(length(invalid_rows) > 0, invalid_rows[1], 0)

  final_rows <- metrics$final_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  final_rows <- ifelse(length(final_rows) > 0, final_rows[1], 0)

  missing_person_id_rows <- metrics$missing_person_id %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  missing_person_id_rows <- ifelse(length(missing_person_id_rows) > 0, missing_person_id_rows[1], 0)

  referential_integrity_violations <- metrics$referential_integrity_violations %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  referential_integrity_violations <- ifelse(length(referential_integrity_violations) > 0, referential_integrity_violations[1], 0)

  drilldown$total_initial_rows <- valid_rows + invalid_rows
  drilldown$valid_rows <- valid_rows
  drilldown$invalid_rows <- invalid_rows
  drilldown$final_rows <- final_rows
  drilldown$missing_person_id_rows <- missing_person_id_rows
  drilldown$referential_integrity_violations <- referential_integrity_violations

  # Type concept breakdown
  drilldown$type_concepts <- metrics$type_concepts_grouped %>%
    dplyr::filter(table_name == !!table_name)

  # Invalid columns (removed during processing)
  drilldown$invalid_columns <- metrics$invalid_columns %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(column_name)

  # Source vocabularies
  drilldown$source_vocabularies <- metrics$source_vocabularies %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::arrange(desc(count))

  # Target vocabularies
  drilldown$target_vocabularies <- metrics$target_vocabularies %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::arrange(desc(count))

  # Vocabulary harmonization (transitions involving this table)
  drilldown$transitions <- metrics$table_transitions %>%
    dplyr::filter(source_table == !!table_name | target_table == !!table_name)

  return(drilldown)
}

# ------------------------------------------------------------------------------
# 9. MAIN EXECUTION FUNCTION
# ------------------------------------------------------------------------------

#' Generate OMOP Delivery Report
#' @param delivery_report_path Path to delivery_report.csv
#' @param dqd_results_path Path to dqd_results.csv
#' @param output_path Path for output HTML file
generate_omop_report <- function(
    delivery_report_path = "delivery_report.csv",
    dqd_results_path = "dqd_results.csv",
    output_path = "omop_delivery_report.html"
) {

  message("\n========================================")
  message("OMOP Delivery Report Generator")
  message("========================================\n")

  # Load data
  message("Loading data...")
  delivery_data <- load_delivery_report(delivery_report_path)
  dqd_data <- load_dqd_results(dqd_results_path)

  # Parse metrics
  message("\nParsing metrics...")
  metrics <- parse_delivery_metrics(delivery_data)

  # Process type concepts
  message("Processing type concepts...")
  metrics$type_concepts_grouped <- group_type_concepts(metrics$type_concepts)

  # Calculate DQD scores
  message("Calculating DQD scores...")
  dqd_scores <- list(
    overall = calculate_overall_dqd_score(dqd_data),
    grid = create_dqd_grid(dqd_data)
  )

  # Identify non-delivered tables
  message("Identifying table delivery status...")
  non_delivered_tables <- identify_non_delivered_tables(metrics)

  message("\nData processing complete!")
  message("\nSummary:")
  message("  - Site: ", metrics$metadata$site)
  message("  - Delivery date: ", metrics$metadata$delivery_date)
  message("  - Overall DQD score: ", dqd_scores$overall, "%")
  message("  - Tables delivered: ", nrow(metrics$valid_tables))
  message("  - Tables not delivered: ", length(non_delivered_tables))

  # Store processed data for report generation
  report_data <- list(
    metrics = metrics,
    dqd_data = dqd_data,
    dqd_scores = dqd_scores,
    non_delivered_tables = non_delivered_tables,
    table_groups = get_table_groups()
  )

  message("\nGenerating HTML report...")

  # Render R Markdown report
  rmarkdown::render(
    input = "report_template.Rmd",
    output_file = output_path,
    params = list(report_data = report_data),
    envir = new.env()
  )

  message("\nReport generated: ", output_path)
  message("\n========================================\n")

  return(invisible(report_data))
}
