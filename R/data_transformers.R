# ==============================================================================
# Data Transformation Functions
# ==============================================================================
# Transform raw metrics into display-ready data structures.
# All functions are pure and testable.

# ==============================================================================
# Data Parsing
# ==============================================================================

#' Parse delivery report metrics from CSV data
#'
#' Extracts all metrics from the delivery report CSV into a structured list.
#'
#' @param delivery_data Data frame with name, value_as_string, value_as_number columns
#' @return List containing all parsed metrics
parse_delivery_metrics <- function(delivery_data) {

  metrics <- list()

  # Parse valid column names
  metrics$valid_columns <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Valid column name:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid column name: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Valid column name: (\\w+)\\.(\\w+)")[, 3]
    ) |>
    dplyr::select(table_name, column_name)

  # Parse invalid column names
  metrics$invalid_columns <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Invalid column name:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid column name: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Invalid column name: (\\w+)\\.(\\w+)")[, 3]
    ) |>
    dplyr::select(table_name, column_name)

  # Parse valid table names
  metrics$valid_tables <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Valid table name:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid table name: (\\w+)")[, 2]
    ) |>
    dplyr::select(table_name) |>
    dplyr::distinct()

  # Parse invalid table names
  metrics$invalid_tables <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Invalid table name:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid table name: (\\w+)")[, 2]
    ) |>
    dplyr::select(table_name) |>
    dplyr::distinct()

  # Parse valid row counts
  metrics$valid_row_counts <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Valid row count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Valid row count: (\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, count)

  # Parse invalid row counts
  metrics$invalid_row_counts <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Invalid row count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid row count: (\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, count)

  # Parse final row counts
  metrics$final_row_counts <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Final row count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Final row count: (\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, count)

  # Parse type concept breakdowns
  metrics$type_concepts <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Type concept breakdown:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Type concept breakdown: (\\w+)")[, 2],
      type_concept = value_as_string,
      count = value_as_number
    ) |>
    dplyr::select(table_name, type_concept, count)

  # Parse source vocabulary breakdowns
  metrics$source_vocabularies <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Source vocabulary breakdown:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Source vocabulary breakdown: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Source vocabulary breakdown: (\\w+)\\.(\\w+)")[, 3],
      vocabulary = value_as_string,
      count = value_as_number
    ) |>
    dplyr::select(table_name, column_name, vocabulary, count)

  # Parse target vocabulary breakdowns
  metrics$target_vocabularies <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Target vocabulary breakdown:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Target vocabulary breakdown: (\\w+)\\.(\\w+)")[, 2],
      column_name = stringr::str_match(name, "Target vocabulary breakdown: (\\w+)\\.(\\w+)")[, 3],
      vocabulary = value_as_string,
      count = value_as_number
    ) |>
    dplyr::select(table_name, column_name, vocabulary, count)

  # Parse vocab harmonization table transitions
  metrics$table_transitions <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization table transition:")) |>
    dplyr::mutate(
      source_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 2],
      target_table = stringr::str_match(name, "Vocab harmonization table transition: (\\w+) to (\\w+)")[, 3],
      count = value_as_number
    ) |>
    dplyr::select(source_table, target_table, count)

  # Parse vocab harmonization statuses (exclude "domain check" per requirements)
  metrics$harmonization_statuses <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization status:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization status: (\\w+) - ")[, 2],
      status = stringr::str_match(name, "Vocab harmonization status: \\w+ - (.+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::filter(!stringr::str_detect(tolower(status), "domain check")) |>
    dplyr::select(table_name, status, count)

  # Parse vocab harmonization same-table mappings
  metrics$same_table_mappings <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization same-table mapping:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization same-table mapping: (\\w+) - ")[, 2],
      mapping = stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 1],
      source_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 2]),
      target_multiplier = as.numeric(stringr::str_match(name, "Vocab harmonization same-table mapping: \\w+ - (\\d+):(\\d+)")[, 3]),
      total_rows = value_as_number,
      rows_added = total_rows * ((target_multiplier - 1) / target_multiplier)
    ) |>
    dplyr::select(table_name, mapping, source_multiplier, target_multiplier, total_rows, rows_added)

  # Parse vocab harmonization row dispositions
  metrics$row_dispositions <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Vocab harmonization row disposition:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Vocab harmonization row disposition: (\\w+) - ")[, 2],
      disposition = stringr::str_match(name, "Vocab harmonization row disposition: \\w+ - (.+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, disposition, count)

  # Parse missing person_id metrics
  metrics$missing_person_id <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "Number of rows removed due to missing person_id values:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Number of rows removed due to missing person_id values: (\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, count)

  # Parse person_id referential integrity violations
  metrics$referential_integrity_violations <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Person_id referential integrity violation count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Person_id referential integrity violation count: (\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, count)

  # Parse default date value counts
  metrics$default_date_values <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Date/datetime default value count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Date/datetime default value count: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Date/datetime default value count: \\w+\\.(\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::select(table_name, column_name, count)

  # Parse invalid concept_id counts
  metrics$invalid_concepts <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Invalid concept_id count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Invalid concept_id count: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Invalid concept_id count: \\w+\\.(\\w+)")[, 2],
      count = value_as_number
    ) |>
    dplyr::filter(!stringr::str_detect(column_name, "_source_concept_id$")) |>
    dplyr::select(table_name, column_name, count)

  # Parse missing columns
  metrics$missing_columns <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Missing column:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Missing column: (\\w+)\\.")[, 2],
      column_name = stringr::str_match(name, "Missing column: \\w+\\.(\\w+)")[, 2]
    ) |>
    dplyr::select(table_name, column_name)

  # Parse technical metadata
  delivery_date_raw <- delivery_data |> dplyr::filter(name == "Delivery date") |> dplyr::pull(value_as_string)
  delivery_date_formatted <- tryCatch({
    date_obj <- as.Date(delivery_date_raw, format = "%m/%d/%y")
    if (is.na(date_obj)) {
      date_obj <- as.Date(delivery_date_raw, format = "%Y-%m-%d")
    }
    format(date_obj, "%Y-%m-%d")
  }, error = function(e) {
    delivery_date_raw
  })

  processing_date_raw <- delivery_data |> dplyr::filter(name == "Delivery processing date") |> dplyr::pull(value_as_string)
  processing_date_formatted <- tryCatch({
    date_obj <- as.Date(processing_date_raw, format = "%m/%d/%y")
    if (is.na(date_obj)) {
      date_obj <- as.Date(processing_date_raw, format = "%Y-%m-%d")
    }
    format(date_obj, "%Y-%m-%d")
  }, error = function(e) {
    processing_date_raw
  })

  metrics$metadata <- list(
    site = delivery_data |> dplyr::filter(name == "Site") |> dplyr::pull(value_as_string),
    delivery_date = delivery_date_formatted,
    processing_date = processing_date_formatted,
    delivered_cdm_version = delivery_data |> dplyr::filter(name == "Delivered CDM version") |> dplyr::pull(value_as_string),
    delivered_vocab_version = delivery_data |> dplyr::filter(name == "Delivered vocabulary version") |> dplyr::pull(value_as_string),
    standardized_cdm_version = delivery_data |> dplyr::filter(name == "Standardized to CDM version") |> dplyr::pull(value_as_string),
    standardized_vocab_version = delivery_data |> dplyr::filter(name == "Standardized to vocabulary version") |> dplyr::pull(value_as_string),
    pipeline_version = delivery_data |> dplyr::filter(name == "Pipeline file processor version") |> dplyr::pull(value_as_string),
    file_format = delivery_data |> dplyr::filter(name == "File delivery format") |> dplyr::pull(value_as_string)
  )

  metrics$missing_person_id_count <- delivery_data |>
    dplyr::filter(name == "Number of persons with missing person_id") |>
    dplyr::pull(value_as_number)

  if (length(metrics$missing_person_id_count) == 0) {
    metrics$missing_person_id_count <- 0
  }

  # Parse time series row counts
  metrics$time_series <- delivery_data |>
    dplyr::filter(stringr::str_detect(name, "^Time series row count:")) |>
    dplyr::mutate(
      table_name = stringr::str_match(name, "Time series row count: (\\w+)\\.(\\d+)")[, 2],
      year = as.integer(stringr::str_match(name, "Time series row count: (\\w+)\\.(\\d+)")[, 3]),
      count = value_as_number
    ) |>
    dplyr::select(table_name, year, count) |>
    dplyr::arrange(table_name, year)

  return(metrics)
}

#' Apply type concept grouping rules
#'
#' Categorizes OMOP type concepts into broader groups (EHR, Claims, Disease registry,
#' Patient reported, Unlabeled, Other) based on naming patterns.
#'
#' @param type_concepts Data frame with type_concept column
#' @return Data frame with added type_group column
group_type_concepts <- function(type_concepts) {

  type_concepts |>
    dplyr::mutate(
      type_group = dplyr::case_when(
        # Unlabeled group
        is.na(type_concept) |
          type_concept == "" |
          type_concept == "0" |
          tolower(type_concept) == "no matching concept" ~ "Unlabeled",
        # EHR group (case-sensitive)
        stringr::str_detect(type_concept, "EHR") ~ "EHR",
        # Claims group (case-insensitive)
        stringr::str_detect(tolower(type_concept), "claim") ~ "Claims",
        stringr::str_detect(tolower(type_concept), "payer system record") ~ "Claims",
        # Disease registry group
        type_concept %in% c("Registry", "Tumor Registry") ~ "Disease registry",
        # Patient reported group
        type_concept %in% c("Patient self-report", "Patient self-tested",
                            "Patient filled survey", "Survey",
                            "Patient Self-Reported Medication") ~ "Patient reported",
        # Other
        TRUE ~ "Other"
      )
    )
}

# ==============================================================================
# Data Transformation
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

#' Prepare complete table data for a single table
#'
#' Aggregates all metrics for one table into a single data structure.
#' This is the main workhorse function for table-level data preparation.
#'
#' @param table_name Character table name
#' @param metrics List of metric data frames from parse_delivery_report
#' @param dqd_score Numeric DQD score for this table (or NA)
#' @return List with all table metrics and calculated values
prepare_table_data <- function(table_name, metrics, dqd_score) {

  # Extract basic counts
  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  invalid_rows <- get_table_count(metrics$invalid_row_counts, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)

  # Special handling for person table missing person_id
  missing_rows <- if (table_name == "person") {
    metrics$missing_person_id_count
  } else {
    get_table_count(metrics$missing_person_id, table_name)
  }

  # Quality metrics
  referential_integrity_violations <- get_table_count(metrics$referential_integrity_violations, table_name)
  default_date_rows <- get_table_count_sum(metrics$default_date_values, table_name)
  invalid_concept_rows <- get_table_count_sum(metrics$invalid_concepts, table_name)

  # Calculate derived counts
  initial_rows <- valid_rows + invalid_rows + missing_rows
  quality_issues <- calculate_quality_issues(invalid_rows, missing_rows)

  # Vocabulary harmonization metrics
  # Note: same_table_mappings uses 'total_rows' not 'count'
  same_table_result_rows <- if (is.null(metrics$same_table_mappings) || nrow(metrics$same_table_mappings) == 0) {
    0
  } else {
    total <- metrics$same_table_mappings |>
      dplyr::filter(table_name == !!table_name) |>
      dplyr::summarise(total = sum(total_rows, na.rm = TRUE)) |>
      dplyr::pull(total)
    ifelse(length(total) > 0, total[1], 0)
  }

  transitions <- metrics$table_transitions |>
    dplyr::filter(source_table == !!table_name | target_table == !!table_name)

  transitions_in <- transitions |>
    dplyr::filter(target_table == !!table_name, source_table != !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  transitions_in <- ifelse(length(transitions_in) > 0, transitions_in[1], 0)

  rows_out <- transitions |>
    dplyr::filter(source_table == !!table_name, target_table != !!table_name) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  rows_out <- ifelse(length(rows_out) > 0, rows_out[1], 0)

  # Calculate harmonization impact
  is_harmonized <- is_harmonized_table(table_name)
  harmonization <- if (is_harmonized) {
    calculate_harmonization(same_table_result_rows, valid_rows, transitions_in)
  } else {
    0
  }

  # Calculate percentages
  default_date_percent <- calculate_percentage(default_date_rows, final_rows)
  invalid_concept_percent <- calculate_percentage(invalid_concept_rows, final_rows)
  missing_person_id_percent <- calculate_percentage(missing_rows, initial_rows)
  referential_integrity_percent <- calculate_percentage(referential_integrity_violations, final_rows)
  invalid_rows_percent <- calculate_percentage(invalid_rows, initial_rows)

  # Extract related data
  type_concepts <- metrics$type_concepts_grouped |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group, desc(count)) |>
    dplyr::mutate(type_group = as.character(type_group))

  invalid_columns <- metrics$invalid_columns |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(column_name)
  invalid_columns <- if (length(invalid_columns) == 0) list() else as.list(invalid_columns)

  missing_columns <- metrics$missing_columns |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(column_name)
  missing_columns <- if (length(missing_columns) == 0) list() else as.list(missing_columns)

  source_vocab <- metrics$source_vocabularies |>
    dplyr::filter(table_name == !!table_name)

  target_vocab <- metrics$target_vocabularies |>
    dplyr::filter(table_name == !!table_name)

  harmonization_statuses <- metrics$harmonization_statuses |>
    dplyr::filter(table_name == !!table_name)

  dispositions <- metrics$row_dispositions |>
    dplyr::filter(table_name == !!table_name)

  same_table_mappings <- metrics$same_table_mappings |>
    dplyr::filter(table_name == !!table_name)

  # Delivery status
  delivered <- table_name %in% metrics$valid_tables$table_name

  # Validation: check if type concepts sum to final rows
  type_concept_total <- type_concepts |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  type_concept_total <- ifelse(length(type_concept_total) > 0, type_concept_total[1], 0)

  has_transitions <- (transitions_in > 0 || same_table_result_rows > 0)

  # Determine expected final count and validation status
  if (type_concept_total > 0) {
    expected_final <- type_concept_total
    counts_valid <- (type_concept_total == final_rows)
  } else if (valid_rows == 0 && !has_transitions) {
    expected_final <- 0
    counts_valid <- (final_rows == 0)
  } else {
    expected_final <- final_rows
    counts_valid <- TRUE
  }

  # Skip validation warnings for non-harmonized tables
  if (!is_harmonized) {
    counts_valid <- TRUE
  }

  # Return comprehensive table data
  list(
    name = table_name,
    delivered = delivered,
    valid_rows = valid_rows,
    invalid_rows = invalid_rows,
    initial_rows = initial_rows,
    final_rows = final_rows,
    missing_person_id_rows = missing_rows,
    referential_integrity_violations = referential_integrity_violations,
    harmonization = harmonization,
    transitions_in = transitions_in,
    rows_out = rows_out,
    same_table_result_rows = same_table_result_rows,
    default_date_rows = default_date_rows,
    invalid_concept_rows = invalid_concept_rows,
    default_date_percent = default_date_percent,
    invalid_concept_percent = invalid_concept_percent,
    missing_person_id_percent = missing_person_id_percent,
    referential_integrity_percent = referential_integrity_percent,
    invalid_rows_percent = invalid_rows_percent,
    quality_issues = quality_issues,
    type_concepts = type_concepts,
    invalid_columns = invalid_columns,
    missing_columns = missing_columns,
    source_vocabularies = source_vocab,
    target_vocabularies = target_vocab,
    transitions = transitions,
    harmonization_statuses = harmonization_statuses,
    dispositions = dispositions,
    same_table_mappings = same_table_mappings,
    dqd_score = dqd_score,
    counts_valid = counts_valid,
    expected_final = expected_final
  )
}

#' Prepare table data for all tables
#'
#' @param metrics List of metric data frames
#' @param table_dqd_scores Named list of DQD scores per table
#' @return List of table data structures
prepare_all_table_data <- function(metrics, table_dqd_scores) {
  # Get all unique tables from valid_tables
  all_tables <- unique(metrics$valid_tables$table_name)

  # Prepare data for each table
  table_data_list <- lapply(all_tables, function(table_name) {
    dqd_score <- table_dqd_scores[[table_name]]
    if (is.null(dqd_score)) dqd_score <- NA_real_

    prepare_table_data(table_name, metrics, dqd_score)
  })

  names(table_data_list) <- all_tables
  table_data_list
}

#' Prepare group-level type concept summary
#'
#' Aggregates type concepts for a table group with percentages.
#'
#' @param metrics List of metric data frames
#' @param group_tables Character vector of table names in the group
#' @return Data frame with type concept summary
prepare_group_type_concepts <- function(metrics, group_tables) {
  # Filter to tables in this group
  group_type_concepts <- metrics$type_concepts_grouped |>
    dplyr::filter(table_name %in% group_tables)

  if (nrow(group_type_concepts) == 0) {
    return(data.frame(
      type_group = character(),
      count = integer(),
      percent = numeric()
    ))
  }

  # Aggregate by type group
  summary <- group_type_concepts |>
    dplyr::group_by(type_group) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Calculate percentages
  total <- sum(summary$count, na.rm = TRUE)
  summary <- summary |>
    dplyr::mutate(
      percent = if (total > 0) (count / total) * 100 else 0
    ) |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group) |>
    dplyr::mutate(type_group = as.character(type_group))

  return(summary)
}

#' Prepare group-level transition statistics
#'
#' Aggregates vocabulary transitions for a table group.
#'
#' @param metrics List of metric data frames
#' @param group_tables Character vector of table names in the group
#' @return List with transition statistics
prepare_group_transitions <- function(metrics, group_tables) {
  # Filter to transitions involving tables in this group
  group_transitions <- metrics$table_transitions |>
    dplyr::filter(source_table %in% group_tables | target_table %in% group_tables)

  if (nrow(group_transitions) == 0) {
    return(list(
      total_rows = 0,
      same_table_count = 0,
      cross_table_count = 0,
      transitions = data.frame()
    ))
  }

  # Calculate transition statistics
  total_rows <- sum(group_transitions$count, na.rm = TRUE)

  same_table_transitions <- group_transitions |>
    dplyr::filter(source_table == target_table)
  same_table_count <- sum(same_table_transitions$count, na.rm = TRUE)

  cross_table_transitions <- group_transitions |>
    dplyr::filter(source_table != target_table)
  cross_table_count <- sum(cross_table_transitions$count, na.rm = TRUE)

  list(
    total_rows = total_rows,
    same_table_count = same_table_count,
    cross_table_count = cross_table_count,
    transitions = group_transitions
  )
}

#' Calculate total number of participants
#'
#' Sums all rows in person table (valid + invalid).
#'
#' @param metrics List of metric data frames
#' @return Integer participant count
calculate_num_participants <- function(metrics) {
  valid <- get_table_count(metrics$valid_row_counts, "person")
  invalid <- get_table_count(metrics$invalid_row_counts, "person")
  valid + invalid
}

#' Calculate total rows removed due to missing person_id
#'
#' @param metrics List of metric data frames
#' @return Integer total rows removed
calculate_total_rows_removed <- function(metrics) {
  if (is.null(metrics$missing_person_id) || nrow(metrics$missing_person_id) == 0) {
    return(0)
  }

  total <- sum(metrics$missing_person_id$count, na.rm = TRUE)
  return(total)
}

#' Prepare overall type concept summary (dataset-wide)
#'
#' @param metrics List of metric data frames
#' @return Data frame with type concept summary
prepare_overall_type_concepts <- function(metrics) {
  if (is.null(metrics$type_concepts_grouped) || nrow(metrics$type_concepts_grouped) == 0) {
    return(data.frame(
      type_group = character(),
      count = integer(),
      percent = numeric()
    ))
  }

  # Aggregate across all tables
  summary <- metrics$type_concepts_grouped |>
    dplyr::group_by(type_group) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Calculate percentages
  total <- sum(summary$count, na.rm = TRUE)
  summary <- summary |>
    dplyr::mutate(
      percent = if (total > 0) (count / total) * 100 else 0
    ) |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group) |>
    dplyr::mutate(type_group = as.character(type_group))

  return(summary)
}

#' Prepare overall transition statistics (dataset-wide)
#'
#' @param metrics List of metric data frames
#' @return List with transition statistics
prepare_overall_transitions <- function(metrics) {
  if (is.null(metrics$table_transitions) || nrow(metrics$table_transitions) == 0) {
    return(list(
      total_rows = 0,
      same_table_count = 0,
      cross_table_count = 0
    ))
  }

  total_rows <- sum(metrics$table_transitions$count, na.rm = TRUE)

  same_table_count <- metrics$table_transitions |>
    dplyr::filter(source_table == target_table) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  same_table_count <- ifelse(length(same_table_count) > 0, same_table_count[1], 0)

  cross_table_count <- metrics$table_transitions |>
    dplyr::filter(source_table != target_table) |>
    dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
    dplyr::pull(total)
  cross_table_count <- ifelse(length(cross_table_count) > 0, cross_table_count[1], 0)

  list(
    total_rows = total_rows,
    same_table_count = same_table_count,
    cross_table_count = cross_table_count
  )
}

#' Create empty metrics structure
#'
#' Used when delivery data is not available.
#'
#' @return List with empty metric structures
create_empty_metrics <- function() {
  list(
    metadata = list(
      site = "Unknown",
      delivery_date = "Unknown",
      processing_date = "Unknown",
      delivered_cdm_version = "Unknown",
      standardized_cdm_version = "Unknown",
      delivered_vocab_version = "Unknown",
      standardized_vocab_version = "Unknown",
      file_format = "unknown",
      pipeline_version = "Unknown"
    ),
    valid_tables = data.frame(table_name = character()),
    valid_row_counts = data.frame(table_name = character(), count = integer()),
    invalid_row_counts = data.frame(table_name = character(), count = integer()),
    final_row_counts = data.frame(table_name = character(), count = integer()),
    missing_person_id = data.frame(table_name = character(), count = integer()),
    missing_person_id_count = 0,
    default_date_values = data.frame(table_name = character(), column_name = character(), count = integer()),
    invalid_concepts = data.frame(table_name = character(), column_name = character(), count = integer()),
    referential_integrity_violations = data.frame(table_name = character(), count = integer()),
    invalid_columns = data.frame(table_name = character(), column_name = character()),
    missing_columns = data.frame(table_name = character(), column_name = character()),
    same_table_mappings = data.frame(table_name = character(), total_rows = integer()),
    table_transitions = data.frame(source_table = character(), target_table = character(), count = integer()),
    source_vocabularies = data.frame(table_name = character(), vocabulary = character(), count = integer()),
    target_vocabularies = data.frame(table_name = character(), vocabulary = character(), count = integer()),
    type_concepts = data.frame(table_name = character(), type_concept = character(), count = integer()),
    type_concepts_grouped = data.frame(table_name = character(), type_group = character(), count = integer()),
    harmonization_statuses = data.frame(table_name = character(), status = character(), count = integer()),
    row_dispositions = data.frame(table_name = character(), disposition = character(), count = integer()),
    time_series = data.frame(year = integer(), table_name = character(), count = integer())
  )
}

#' Ensure all type concept groups are present
#'
#' Fills in missing type concept groups with zero counts.
#' This ensures consistent display even when some groups have no data.
#'
#' @param type_concept_data Data frame with type_group and count columns
#' @param config Configuration list (default: default_config())
#' @return Data frame with all type groups present
ensure_all_type_groups <- function(type_concept_data, config = default_config()) {
  all_groups <- get_type_concept_group_order(config)

  # Create a complete template with all groups
  complete_groups <- tibble::tibble(
    type_group = all_groups,
    count = 0
  )

  # Merge with actual data, keeping all groups
  result <- complete_groups |>
    dplyr::left_join(
      type_concept_data |> dplyr::select(type_group, count),
      by = "type_group",
      suffix = c("_template", "_actual")
    ) |>
    dplyr::mutate(
      count = dplyr::coalesce(count_actual, count_template)
    ) |>
    dplyr::select(-count_template, -count_actual)

  # If original data had additional columns, merge them back
  extra_cols <- setdiff(names(type_concept_data), c("type_group", "count"))
  if (length(extra_cols) > 0) {
    result <- result |>
      dplyr::left_join(
        type_concept_data |> dplyr::select(type_group, dplyr::all_of(extra_cols)),
        by = "type_group"
      )
  }

  # Ensure proper ordering
  result |>
    dplyr::mutate(type_group = factor(type_group, levels = all_groups)) |>
    dplyr::arrange(type_group) |>
    dplyr::mutate(type_group = as.character(type_group))
}

# ==============================================================================
# Section Data Preparation
# ==============================================================================

#' Prepare data for time series section
#'
#' Calculates year ranges and formats time series data for template rendering.
#'
#' @param metrics List of parsed metrics
#' @return List with template variables or NULL if no data
prepare_time_series_data <- function(metrics) {
  if (is.null(metrics) || is.null(metrics$time_series) || nrow(metrics$time_series) == 0) {
    return(NULL)
  }

  # Extract delivery date and calculate year ranges
  delivery_date_str <- metrics$metadata$delivery_date

  # Parse delivery date to get year
  delivery_year <- tryCatch({
    if (is.na(delivery_date_str) || delivery_date_str == "Unknown" || delivery_date_str == "") {
      as.integer(format(Sys.Date(), "%Y"))
    } else {
      as.integer(format(as.Date(delivery_date_str), "%Y"))
    }
  }, error = function(e) {
    as.integer(format(Sys.Date(), "%Y"))
  })

  # Calculate year ranges
  recent_start_year <- delivery_year - 15
  recent_end_year <- delivery_year - 1
  historical_start_year <- 1970
  historical_end_year <- delivery_year

  # Convert time series data to JSON for JavaScript
  time_series_json <- jsonlite::toJSON(metrics$time_series, dataframe = "rows")

  list(
    recent_start_year = recent_start_year,
    recent_end_year = recent_end_year,
    historical_start_year = historical_start_year,
    historical_end_year = historical_end_year,
    delivery_year = delivery_year,
    time_series_json = as.character(time_series_json)
  )
}

#' Prepare data for vocabulary harmonization section
#'
#' Aggregates source and target vocabularies for top 10 display.
#'
#' @param metrics List of parsed metrics
#' @return List with source and target vocabulary data
prepare_vocab_harmonization_data <- function(metrics) {
  if (is.null(metrics) || is.null(metrics$source_vocabularies) || is.null(metrics$target_vocabularies)) {
    return(list(
      source_vocab_rows = list(),
      target_vocab_rows = list(),
      delivered_vocab_version = "Unknown",
      standardized_vocab_version = "Unknown"
    ))
  }

  # Overall source vocabularies (top 10)
  overall_source_vocab <- metrics$source_vocabularies |>
    dplyr::group_by(vocabulary) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count)) |>
    head(10)

  source_vocab_rows <- if (nrow(overall_source_vocab) > 0) {
    lapply(1:nrow(overall_source_vocab), function(i) {
      list(
        vocabulary = overall_source_vocab$vocabulary[i],
        count = format(overall_source_vocab$count[i], big.mark = ",")
      )
    })
  } else {
    list()
  }

  # Overall target vocabularies (top 10)
  overall_target_vocab <- metrics$target_vocabularies |>
    dplyr::group_by(vocabulary) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count)) |>
    head(10)

  target_vocab_rows <- if (nrow(overall_target_vocab) > 0) {
    lapply(1:nrow(overall_target_vocab), function(i) {
      list(
        vocabulary = overall_target_vocab$vocabulary[i],
        count = format(overall_target_vocab$count[i], big.mark = ",")
      )
    })
  } else {
    list()
  }

  list(
    source_vocab_rows = source_vocab_rows,
    target_vocab_rows = target_vocab_rows,
    delivered_vocab_version = metrics$metadata$delivered_vocab_version,
    standardized_vocab_version = metrics$metadata$standardized_vocab_version
  )
}

#' Prepare data for delivery report section
#'
#' Formats table data with all displayformatting, warnings, and icons.
#'
#' @param metrics List of parsed metrics
#' @param table_groups Named list of table groups
#' @param group_dqd_scores Named list of DQD scores per group
#' @param num_participants Integer number of participants
#' @return List with dropdown options and group contents data
prepare_delivery_report_data <- function(metrics, table_groups, group_dqd_scores, num_participants) {
  # Prepare dropdown options
  dropdown_options_data <- lapply(names(table_groups), function(group_name) {
    list(
      group_name = group_name,
      selected = if (group_name == "Clinical Data") " selected" else ""
    )
  })

  # Prepare each table group
  group_contents_data <- lapply(names(table_groups), function(group_name) {
    group_tables <- table_groups[[group_name]]
    group_id <- gsub(" ", "-", tolower(group_name))
    display_style <- if (group_name == "Clinical Data") "" else "display: none;"
    
    # Prepare DQD note
    group_dqd_score <- group_dqd_scores[[group_name]]
    dqd_note <- if (!is.na(group_dqd_score)) {
      sprintf('<p class="dqd-score-text"><strong>Data Quality Score for this group:</strong> <span class="dqd-inline">%s%%</span></p>',
              group_dqd_score)
    } else {
      '<p class="dqd-score-text"><strong>Data Quality Score:</strong> <span class="text-muted">Not available</span></p>'
    }
    
    # Prepare type concept subheader
    type_concept_subheader <- if (group_name == "All Tables") {
      "All Tables"
    } else {
      sprintf("%s Tables", group_name)
    }
    
    # Prepare table rows
    table_rows_data <- lapply(group_tables, function(tbl) {
      prepare_delivery_table_row(tbl, metrics, num_participants)
    })
    
    list(
      group_name = group_name,
      group_id = group_id,
      display_style = display_style,
      dqd_note = dqd_note,
      type_concept_subheader = type_concept_subheader,
      table_rows_data = table_rows_data
    )
  })
  
  list(
    dropdown_options_data = dropdown_options_data,
    group_contents_data = group_contents_data
  )
}

#' Prepare formatted data for a single table row in delivery report
#'
#' @param table_name Character table name
#' @param metrics List of parsed metrics
#' @param num_participants Integer number of participants
#' @return List with all formatted fields for template
prepare_delivery_table_row <- function(table_name, metrics, num_participants) {
  # Get raw counts
  valid_rows <- get_table_count(metrics$valid_row_counts, table_name)
  invalid_rows <- get_table_count(metrics$invalid_row_counts, table_name)
  final_rows <- get_table_count(metrics$final_row_counts, table_name)
  
  # Get missing person ID rows
  missing_rows <- get_table_count(metrics$missing_person_id, table_name)
  if (missing_rows == 0 && table_name == "person") {
    missing_rows <- metrics$missing_person_id_count
  }
  
  initial_rows <- valid_rows + invalid_rows + missing_rows
  quality_issues <- invalid_rows + missing_rows
  
  # Calculate harmonization
  harmonized_tables <- c("visit_occurrence", "condition_occurrence", "drug_exposure",
                        "procedure_occurrence", "device_exposure", "measurement",
                        "observation", "note", "specimen")
  
  is_harmonized <- table_name %in% harmonized_tables
  
  if (is_harmonized) {
    # Note: same_table_mappings uses 'total_rows' column not 'count'
    if (is.null(metrics$same_table_mappings) || nrow(metrics$same_table_mappings) == 0) {
      same_table_result_rows <- 0
    } else {
      same_table_result_rows <- metrics$same_table_mappings |>
        dplyr::filter(table_name == !!table_name) |>
        dplyr::summarise(total = sum(total_rows, na.rm = TRUE)) |>
        dplyr::pull(total)
      same_table_result_rows <- ifelse(length(same_table_result_rows) > 0, same_table_result_rows[1], 0)
    }
    
    transitions_in <- metrics$table_transitions |>
      dplyr::filter(target_table == !!table_name, source_table != !!table_name) |>
      dplyr::summarise(total = sum(count, na.rm = TRUE)) |>
      dplyr::pull(total)
    transitions_in <- ifelse(length(transitions_in) > 0, transitions_in[1], 0)
    
    harmonization <- same_table_result_rows - valid_rows + transitions_in
  } else {
    harmonization <- 0
  }
  
  # Format harmonization display
  if (!is_harmonized) {
    harmonization_display <- "--"
    harmonization_class <- "harmonization-neutral"
  } else if (harmonization > 0) {
    harmonization_display <- paste0("+", format(harmonization, big.mark = ","))
    harmonization_class <- "harmonization-positive"
  } else if (harmonization < 0) {
    harmonization_display <- format(harmonization, big.mark = ",")
    harmonization_class <- "harmonization-negative"
  } else {
    harmonization_display <- "0"
    harmonization_class <- "harmonization-neutral"
  }
  
  # Format quality issues display
  quality_issues_display <- if (quality_issues > 0) {
    paste0("-", format(quality_issues, big.mark = ","))
  } else {
    "0"
  }
  quality_issues_class <- if (quality_issues > 0) "harmonization-negative" else "harmonization-neutral"
  
  # Calculate row per patient
  row_per_patient <- if (num_participants > 0 && final_rows > 0) {
    sprintf("%.2f", final_rows / num_participants)
  } else {
    "0.00"
  }
  
  # Determine status - table is delivered if it appears in valid_tables
  is_valid_table <- table_name %in% metrics$valid_tables$table_name
  if (is_valid_table) {
    status_text <- "Delivered"
    status_class <- "delivered"
  } else {
    status_text <- "Not Delivered"
    status_class <- "not-delivered"
  }
  
  # Calculate warnings (simplified - would need config access for thresholds)
  has_any_alert <- FALSE
  all_warnings <- ""
  row_class <- if (has_any_alert) "row-warning" else ""
  
  list(
    table_name = table_name,
    row_class = row_class,
    all_warnings = all_warnings,
    status_text = status_text,
    status_class = status_class,
    initial_rows_formatted = format(initial_rows, big.mark = ","),
    quality_issues_display = quality_issues_display,
    quality_issues_class = quality_issues_class,
    harmonization_display = harmonization_display,
    harmonization_class = harmonization_class,
    final_rows_formatted = format(final_rows, big.mark = ","),
    row_per_patient = row_per_patient
  )
}

#' Prepare overview section data
#'
#' Prepares all variables needed for the overview section template.
#'
#' @param metrics List of metrics
#' @param dqd_scores List with DQD scores
#' @param num_participants Integer participant count
#' @param total_rows_removed Integer rows removed count
#' @param has_delivery_data Logical
#' @param has_dqd_data Logical
#' @return List of template variables
prepare_overview_data <- function(metrics, dqd_scores, num_participants, total_rows_removed, has_delivery_data, has_dqd_data) {
  # Format displays
  tables_delivered <- if (has_delivery_data) as.character(nrow(metrics$valid_tables)) else "N/A"
  participants_display <- if (has_delivery_data) format_number(num_participants) else "N/A"
  missing_person_display <- if (has_delivery_data) as.character(metrics$missing_person_id_count) else "N/A"
  rows_removed_display <- if (has_delivery_data) format_number(total_rows_removed) else "N/A"

  # Warning classes and icons
  if (has_delivery_data) {
    missing_warning <- if (metrics$missing_person_id_count > 0) " warning" else " success"
    missing_icon <- if (metrics$missing_person_id_count > 0) '<span class="warning-icon">⚠️</span>' else '<span class="success-icon">✓</span>'
    rows_warning <- if (total_rows_removed > 0) " warning" else " success"
    rows_icon <- if (total_rows_removed > 0) '<span class="warning-icon">⚠️</span>' else '<span class="success-icon">✓</span>'
    person_word <- if (metrics$missing_person_id_count == 1) "Person" else "Persons"
  } else {
    missing_warning <- " neutral"
    missing_icon <- ""
    rows_warning <- " neutral"
    rows_icon <- ""
    person_word <- "Persons"
  }

  # DQD score
  dqd_class <- get_dqd_score_class(dqd_scores$overall)
  dqd_score_display <- if (is.na(dqd_scores$overall)) "N/A" else paste0(dqd_scores$overall, "%")

  list(
    tables_delivered = tables_delivered,
    participants_display = participants_display,
    dqd_class = dqd_class,
    dqd_score_display = dqd_score_display,
    missing_warning = missing_warning,
    missing_icon = missing_icon,
    missing_person_display = missing_person_display,
    person_word = person_word,
    rows_warning = rows_warning,
    rows_icon = rows_icon,
    rows_removed_display = rows_removed_display
  )
}

#' Prepare DQD grid rows data
#'
#' Prepares data for DQD grid table rows.
#'
#' @param grid Data frame with DQD grid data
#' @return List of row data for rendering
prepare_dqd_grid_rows <- function(grid) {
  if (nrow(grid) == 0) {
    return(list())
  }

  # Reshape for display
  grid_wide <- grid |>
    tidyr::pivot_wider(names_from = context, values_from = c(Pass, Fail, Total, `% Pass`))

  categories <- c("Plausibility", "Conformance", "Completeness", "Total")

  rows_data <- lapply(categories, function(cat_name) {
    row_data <- grid_wide |> dplyr::filter(category == cat_name)

    if (nrow(row_data) == 0) return(NULL)

    row_class <- if (cat_name == "Total") ' class="total-row"' else ''

    list(
      row_class = row_class,
      category = cat_name,
      pass_verification = ifelse(is.na(row_data$Pass_Verification), 0, row_data$Pass_Verification),
      fail_verification = ifelse(is.na(row_data$Fail_Verification), 0, row_data$Fail_Verification),
      total_verification = ifelse(is.na(row_data$Total_Verification), 0, row_data$Total_Verification),
      percent_pass_verification = ifelse(is.na(row_data$`% Pass_Verification`), "0%", paste0(row_data$`% Pass_Verification`, "%")),
      pass_validation = ifelse(is.na(row_data$Pass_Validation), 0, row_data$Pass_Validation),
      fail_validation = ifelse(is.na(row_data$Fail_Validation), 0, row_data$Fail_Validation),
      total_validation = ifelse(is.na(row_data$Total_Validation), 0, row_data$Total_Validation),
      percent_pass_validation = ifelse(is.na(row_data$`% Pass_Validation`), "0%", paste0(row_data$`% Pass_Validation`, "%")),
      pass_total = ifelse(is.na(row_data$Pass_Total), 0, row_data$Pass_Total),
      fail_total = ifelse(is.na(row_data$Fail_Total), 0, row_data$Fail_Total),
      total_total = ifelse(is.na(row_data$Total_Total), 0, row_data$Total_Total),
      percent_pass_total = paste0(ifelse(is.na(row_data$`% Pass_Total`), 0, row_data$`% Pass_Total`), "%")
    )
  })

  # Filter out NULL entries
  Filter(Negate(is.null), rows_data)
}
