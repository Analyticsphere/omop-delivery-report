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

  # Prepare all table data
  all_table_data <- prepare_all_table_data(metrics, table_dqd_scores)

  # Prepare group-level data
  groups_data <- lapply(names(table_groups), function(group_name) {
    group_tables <- table_groups[[group_name]]

    list(
      name = group_name,
      tables = group_tables,
      dqd_score = group_dqd_scores[[group_name]],
      type_concepts = prepare_group_type_concepts(metrics, group_tables),
      transitions = prepare_group_transitions(metrics, group_tables)
    )
  })
  names(groups_data) <- names(table_groups)

  # Prepare overall statistics
  overall_type_concepts <- prepare_overall_type_concepts(metrics)
  overall_transitions <- prepare_overall_transitions(metrics)

  # Build the complete data structure
  report_data <- list(
    metadata = metrics$metadata,
    tables = all_table_data,
    groups = groups_data,
    overall_type_concepts = overall_type_concepts,
    overall_transition_stats = overall_transitions
  )

  # Serialize to JSON
  json_string <- jsonlite::toJSON(
    report_data,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null"
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
  top_vocabs <- vocab_data %>%
    dplyr::group_by(vocabulary) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(count)) %>%
    head(top_n)

  # Format for display
  lapply(1:nrow(top_vocabs), function(i) {
    list(
      vocabulary = top_vocabs$vocabulary[i],
      count = format_number(top_vocabs$count[i])
    )
  })
}
