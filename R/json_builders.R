# ==============================================================================
# JSON Data Builders
# ==============================================================================
# Prepare data structures for JavaScript consumption.
# Handles serialization and formatting for the report's interactive features.

#' Build complete JSON data object for JavaScript
#'
#' Serializes pre-calculated report data to JSON format for JavaScript consumption.
#' This is a pure serialization function - all business logic should be done
#' in prepare_report_data() before calling this function.
#'
#' @param report_data List of pre-calculated report data from prepare_report_data()
#' @return Character JSON string
build_report_data_json <- function(report_data) {

  # Add configuration data (colors, ordering)
  report_data$type_colors <- as.list(get_type_concept_colors())
  report_data$table_colors <- as.list(get_table_colors())
  report_data$type_group_order <- get_type_concept_group_order()

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

#' Format vocabulary table rows for display
#'
#' Formats pre-aggregated vocabulary data for display.
#' Aggregation logic should be done in data preparation functions.
#'
#' @param vocab_data Data frame with vocabulary and count columns (already aggregated)
#' @return List of vocabulary rows formatted for display
build_vocab_table_rows <- function(vocab_data) {
  if (is.null(vocab_data) || nrow(vocab_data) == 0) {
    return(list(list(vocabulary = "No data", count = "0")))
  }

  # Format for display (no business logic, just formatting)
  lapply(1:nrow(vocab_data), function(i) {
    list(
      vocabulary = vocab_data$vocabulary[i],
      count = format_number(vocab_data$count[i])
    )
  })
}
