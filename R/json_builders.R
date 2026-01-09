# ==============================================================================
# JSON Data Builders
# ==============================================================================
# Prepare data structures for JavaScript consumption.
# Handles serialization and formatting for the report's interactive features.

#' Build complete JSON data object for JavaScript
#'
#' Creates the REPORT_DATA global object that JavaScript uses for all
#' interactive features (drilldowns, charts, etc.).
#'
#' @param metrics List of metric data frames
#' @param dqd_data Data frame with DQD results
#' @param table_groups Named list of table groups
#' @param group_dqd_scores Named list of DQD scores per group
#' @param table_dqd_scores Named list of DQD scores per table
#' @return Character JSON string
build_report_data_json <- function(metrics, dqd_data, table_groups, group_dqd_scores, table_dqd_scores) {

  # Prepare group-level data
  groups_data <- list()

  for (group_name in names(table_groups)) {
    group_tables <- table_groups[[group_name]]

    # Get data for each table in group
    table_data_list <- lapply(group_tables, function(tbl) {
      prepare_table_data(tbl, metrics, table_dqd_scores[[tbl]])
    })
    names(table_data_list) <- group_tables

    # Group-level vocabularies
    group_source_vocab <- metrics$source_vocabularies |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(vocabulary) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(desc(count))

    group_target_vocab <- metrics$target_vocabularies |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(vocabulary) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(desc(count))

    # Group-level transitions
    group_transitions <- metrics$table_transitions |>
      dplyr::filter(source_table %in% group_tables | target_table %in% group_tables)

    # Group-level type concepts - DETAILED (with type_concept field)
    group_type_concepts_detailed <- metrics$type_concepts_grouped |>
      dplyr::filter(table_name %in% group_tables) |>
      dplyr::group_by(type_group, type_concept) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

    # Aggregate by type_group only and ensure all groups present - SUMMARY
    group_type_concepts_summary <- group_type_concepts_detailed |>
      dplyr::group_by(type_group) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
      ensure_all_type_groups()

    # Add percentages to summary
    total_type_concept_count <- sum(group_type_concepts_summary$count, na.rm = TRUE)
    group_type_concepts_summary <- group_type_concepts_summary |>
      dplyr::mutate(
        percent = if (total_type_concept_count > 0) (count / total_type_concept_count) * 100 else 0
      )

    # Order detailed type concepts by canonical group, then count descending
    group_type_concepts <- group_type_concepts_detailed |>
      dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
      dplyr::arrange(type_group, desc(count)) |>
      dplyr::mutate(type_group = as.character(type_group))

    # Calculate transition statistics for this group
    total_transition_rows <- sum(group_transitions$count, na.rm = TRUE)
    same_table_transitions <- group_transitions |>
      dplyr::filter(source_table == target_table)
    cross_table_transitions <- group_transitions |>
      dplyr::filter(source_table != target_table)
    same_table_count <- sum(same_table_transitions$count, na.rm = TRUE)
    cross_table_count <- sum(cross_table_transitions$count, na.rm = TRUE)

    groups_data[[group_name]] <- list(
      tables = table_data_list,
      dqd_score = group_dqd_scores[[group_name]],
      source_vocabularies = group_source_vocab,
      target_vocabularies = group_target_vocab,
      transitions = group_transitions,
      transition_stats = list(
        total_rows = total_transition_rows,
        same_table_count = same_table_count,
        cross_table_count = cross_table_count
      ),
      type_concepts = group_type_concepts,
      type_concepts_summary = group_type_concepts_summary
    )
  }

  # Add overall transitions for vocabulary harmonization section
  overall_transitions <- metrics$table_transitions

  # Add overall harmonization statuses (summarize across all tables)
  overall_harmonization_statuses <- metrics$harmonization_statuses |>
    dplyr::group_by(status) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count))

  # Add overall type concepts - DETAILED (with type_concept field)
  overall_type_concepts_detailed <- metrics$type_concepts_grouped |>
    dplyr::group_by(type_group, type_concept) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Aggregate by type_group only and ensure all groups present - SUMMARY
  overall_type_concepts_summary <- overall_type_concepts_detailed |>
    dplyr::group_by(type_group) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    ensure_all_type_groups()

  # Add percentages to overall summary
  total_overall_count <- sum(overall_type_concepts_summary$count, na.rm = TRUE)
  overall_type_concepts_summary <- overall_type_concepts_summary |>
    dplyr::mutate(
      percent = if (total_overall_count > 0) (count / total_overall_count) * 100 else 0
    )

  # Order detailed type concepts by canonical group, then count descending
  overall_type_concepts <- overall_type_concepts_detailed |>
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) |>
    dplyr::arrange(type_group, desc(count)) |>
    dplyr::mutate(type_group = as.character(type_group))

  # Calculate overall transition statistics
  total_overall_transition_rows <- sum(overall_transitions$count, na.rm = TRUE)
  overall_same_table_transitions <- overall_transitions |>
    dplyr::filter(source_table == target_table)
  overall_cross_table_transitions <- overall_transitions |>
    dplyr::filter(source_table != target_table)
  overall_same_table_count <- sum(overall_same_table_transitions$count, na.rm = TRUE)
  overall_cross_table_count <- sum(overall_cross_table_transitions$count, na.rm = TRUE)

  # Build the complete data structure
  report_data <- list(
    groups = groups_data,
    type_colors = as.list(get_type_concept_colors()),
    type_group_order = get_type_concept_group_order(),
    overall_transitions = overall_transitions,
    overall_transition_stats = list(
      total_rows = total_overall_transition_rows,
      same_table_count = overall_same_table_count,
      cross_table_count = overall_cross_table_count
    ),
    harmonization_statuses = overall_harmonization_statuses,
    overall_type_concepts = overall_type_concepts,
    overall_type_concepts_summary = overall_type_concepts_summary
  )

  # Serialize to JSON
  json_string <- jsonlite::toJSON(
    report_data,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    pretty = FALSE
  )

  return(as.character(json_string))
}

#' Build table rows JSON for a table group
#'
#' Prepares JSON array of table row data for the delivery report table.
#' Each row contains the metrics displayed in the main table grid.
#'
#' @param group_tables Character vector of table names
#' @param all_table_data List of table data structures
#' @param num_participants Integer total participants
#' @return List of row data (will be serialized to JSON)
build_table_rows_data <- function(group_tables, all_table_data, num_participants) {
  lapply(group_tables, function(table_name) {
    table_data <- all_table_data[[table_name]]
    if (is.null(table_data)) {
      return(NULL)
    }

    # Calculate row-per-patient
    row_per_patient <- calculate_row_per_patient(table_data$final_rows, num_participants)

    # Format harmonization display
    harm_display <- format_harmonization_display(table_data$harmonization, is_harmonized_table(table_name))

    # Format quality issues display
    quality_display <- format_quality_issues_display(table_data$quality_issues)

    # Status badge
    status <- get_status_badge(table_data$delivered)

    list(
      name = table_name,
      status = status$text,
      status_class = status$class,
      initial_rows = format_number(table_data$initial_rows),
      quality_issues = quality_display$text,
      quality_issues_class = quality_display$class,
      harmonization = harm_display$text,
      harmonization_class = harm_display$class,
      final_rows = format_number(table_data$final_rows),
      row_per_patient = row_per_patient
    )
  })
}

#' Build vocabulary table rows for top source/target vocabularies
#'
#' @param vocab_data Data frame with vocabulary and count columns
#' @param top_n Integer number of top vocabularies to include (default: 10)
#' @return List of vocabulary rows
build_vocab_table_rows <- function(vocab_data, top_n = 10) {
  if (is.null(vocab_data) || nrow(vocab_data) == 0) {
    return(list(list(vocabulary = "No data", count = "0")))
  }

  # Aggregate and get top N
  top_vocabs <- vocab_data |>
    dplyr::group_by(vocabulary) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(desc(count)) |>
    head(top_n)

  # Format for display
  lapply(1:nrow(top_vocabs), function(i) {
    list(
      vocabulary = top_vocabs$vocabulary[i],
      count = format_number(top_vocabs$count[i])
    )
  })
}
