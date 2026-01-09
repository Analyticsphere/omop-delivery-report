# ==============================================================================
# Data Transformation Functions
# ==============================================================================
# Transform raw metrics into display-ready data structures.
# All functions are pure and testable.

#' Extract count for a table from metrics
#'
#' Helper to safely extract a count value from a metric data frame.
#'
#' @param metric_df Data frame with table_name and count columns
#' @param table_name Character table name to extract
#' @return Integer count (0 if not found)
get_table_count <- function(metric_df, table_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(0)

  value <- metric_df %>%
    dplyr::filter(table_name == !!table_name) %>%
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

  total <- metric_df %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
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
  same_table_result_rows <- get_table_count_sum(metrics$same_table_mappings, table_name)

  transitions <- metrics$table_transitions %>%
    dplyr::filter(source_table == !!table_name | target_table == !!table_name)

  transitions_in <- transitions %>%
    dplyr::filter(target_table == !!table_name, source_table != !!table_name) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
    dplyr::pull(total)
  transitions_in <- ifelse(length(transitions_in) > 0, transitions_in[1], 0)

  rows_out <- transitions %>%
    dplyr::filter(source_table == !!table_name, target_table != !!table_name) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
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
  type_concepts <- metrics$type_concepts_grouped %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
    dplyr::arrange(type_group, desc(count)) %>%
    dplyr::mutate(type_group = as.character(type_group))

  invalid_columns <- metrics$invalid_columns %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(column_name)
  invalid_columns <- if (length(invalid_columns) == 0) list() else as.list(invalid_columns)

  missing_columns <- metrics$missing_columns %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(column_name)
  missing_columns <- if (length(missing_columns) == 0) list() else as.list(missing_columns)

  source_vocab <- metrics$source_vocabularies %>%
    dplyr::filter(table_name == !!table_name)

  target_vocab <- metrics$target_vocabularies %>%
    dplyr::filter(table_name == !!table_name)

  harmonization_statuses <- metrics$harmonization_statuses %>%
    dplyr::filter(table_name == !!table_name)

  dispositions <- metrics$row_dispositions %>%
    dplyr::filter(table_name == !!table_name)

  same_table_mappings <- metrics$same_table_mappings %>%
    dplyr::filter(table_name == !!table_name)

  # Delivery status
  delivered <- table_name %in% metrics$valid_tables$table_name

  # Validation: check if type concepts sum to final rows
  type_concept_total <- type_concepts %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
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
  group_type_concepts <- metrics$type_concepts_grouped %>%
    dplyr::filter(table_name %in% group_tables)

  if (nrow(group_type_concepts) == 0) {
    return(data.frame(
      type_group = character(),
      count = integer(),
      percent = numeric()
    ))
  }

  # Aggregate by type group
  summary <- group_type_concepts %>%
    dplyr::group_by(type_group) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Calculate percentages
  total <- sum(summary$count, na.rm = TRUE)
  summary <- summary %>%
    dplyr::mutate(
      percent = if (total > 0) (count / total) * 100 else 0
    ) %>%
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
    dplyr::arrange(type_group) %>%
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
  group_transitions <- metrics$table_transitions %>%
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

  same_table_transitions <- group_transitions %>%
    dplyr::filter(source_table == target_table)
  same_table_count <- sum(same_table_transitions$count, na.rm = TRUE)

  cross_table_transitions <- group_transitions %>%
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
  summary <- metrics$type_concepts_grouped %>%
    dplyr::group_by(type_group) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Calculate percentages
  total <- sum(summary$count, na.rm = TRUE)
  summary <- summary %>%
    dplyr::mutate(
      percent = if (total > 0) (count / total) * 100 else 0
    ) %>%
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
    dplyr::arrange(type_group) %>%
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

  same_table_count <- metrics$table_transitions %>%
    dplyr::filter(source_table == target_table) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
    dplyr::pull(total)
  same_table_count <- ifelse(length(same_table_count) > 0, same_table_count[1], 0)

  cross_table_count <- metrics$table_transitions %>%
    dplyr::filter(source_table != target_table) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
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
