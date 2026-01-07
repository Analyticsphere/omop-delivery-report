# ==============================================================================
# OMOP Delivery Report Generator
# ==============================================================================

# ==============================================================================
# MAIN GENERATION FUNCTION
# ==============================================================================

#' Generate complete interactive OMOP delivery report
#'
#' Creates a self-contained HTML report with data quality metrics, table summaries,
#' vocabulary harmonization flows, and interactive drilldowns. No external dependencies.
#' Supports both local filesystem and Google Cloud Storage paths.
#'
#' @param delivery_report_path Character. Path to delivery_report.csv (local or gs:// URI).
#'   Required columns: name, value_as_string, value_as_number
#' @param dqd_results_path Character. Path to dqd_results.csv (local or gs:// URI).
#'   Required columns: checkId, failed, passed, category, cdmTableName
#' @param output_path Character. Output path for HTML file (local or gs:// URI).
#'
#' @return Invisibly returns output path
#'
#' @examples
#' # Local mode
#' generate_full_omop_report(
#'   delivery_report_path = "data/delivery_report.csv",
#'   dqd_results_path = "data/dqd_results.csv",
#'   output_path = "output/report.html"
#' )
#'
#' # GCS mode
#' generate_full_omop_report(
#'   delivery_report_path = "gs://my-bucket/data/delivery_report.csv",
#'   dqd_results_path = "gs://my-bucket/data/dqd_results.csv",
#'   output_path = "gs://my-bucket/reports/report.html"
#' )
#'
#' @export
generate_full_omop_report <- function(
  delivery_report_path = "delivery_report.csv",
  dqd_results_path = "dqd_results.csv",
  output_path = "omop_delivery_report_full.html"
) {

  message("\n" , paste(rep("=", 60), collapse = ""))
  message("OMOP DELIVERY REPORT GENERATOR")
  message(paste(rep("=", 60), collapse = ""), "\n")

  # Load and process data
  message("Loading data files...")
  delivery_data <- load_delivery_report(delivery_report_path)
  dqd_data <- load_dqd_results(dqd_results_path)

  # Check data availability
  has_delivery_data <- !is.null(delivery_data)
  has_dqd_data <- !is.null(dqd_data)

  if (!has_delivery_data && !has_dqd_data) {
    stop("Both input files are missing. Cannot generate report.\n",
         "Delivery report: ", delivery_report_path, "\n",
         "DQD results: ", dqd_results_path)
  }

  if (has_delivery_data) {
    message("Delivery data available")
  } else {
    message("Delivery data NOT available - will use placeholder values")
  }

  if (has_dqd_data) {
    message("DQD data available")
  } else {
    message("DQD data NOT available - will use placeholder values")
  }

  message("Parsing delivery metrics...")
  if (has_delivery_data) {
    metrics <- parse_delivery_metrics(delivery_data)
    metrics$type_concepts_grouped <- group_type_concepts(metrics$type_concepts)
  } else {
    metrics <- create_empty_metrics()
  }

  message("Calculating DQD scores...")
  if (has_dqd_data) {
    dqd_scores <- list(
      overall = calculate_overall_dqd_score(dqd_data),
      grid = create_dqd_grid(dqd_data)
    )
  } else {
    dqd_scores <- create_empty_dqd_scores()
  }

  # Calculate group-level scores
  table_groups <- get_table_groups()
  if (has_dqd_data) {
    group_dqd_scores <- lapply(table_groups, function(tables) {
      calculate_table_group_dqd_score(dqd_data, tables)
    })
    names(group_dqd_scores) <- names(table_groups)

    # Calculate table-level scores
    all_tables <- unique(unlist(table_groups))
    table_dqd_scores <- lapply(all_tables, function(tbl) {
      calculate_table_dqd_score(dqd_data, tbl)
    })
    names(table_dqd_scores) <- all_tables
  } else {
    # Set all group scores to NA when DQD data unavailable
    group_dqd_scores <- lapply(table_groups, function(tables) NA_real_)
    names(group_dqd_scores) <- names(table_groups)

    all_tables <- unique(unlist(table_groups))
    table_dqd_scores <- lapply(all_tables, function(tbl) NA_real_)
    names(table_dqd_scores) <- all_tables
  }

  message("Preparing visualizations...")

  # Summary metrics
  num_participants <- sum(
    ifelse(nrow(metrics$valid_row_counts %>% dplyr::filter(table_name == "person")) > 0,
           metrics$valid_row_counts %>% dplyr::filter(table_name == "person") %>% dplyr::pull(count), 0),
    ifelse(nrow(metrics$invalid_row_counts %>% dplyr::filter(table_name == "person")) > 0,
           metrics$invalid_row_counts %>% dplyr::filter(table_name == "person") %>% dplyr::pull(count), 0)
  )

  total_rows_removed <- sum(metrics$missing_person_id$count, na.rm = TRUE)

  # Type concept summary for overview
  type_concept_summary <- metrics$type_concepts_grouped %>%
    dplyr::group_by(type_group) %>%
    dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(total_count))

  message("Building HTML structure...")
  html_content <- build_complete_html_report(
    metrics = metrics,
    dqd_data = dqd_data,
    dqd_scores = dqd_scores,
    group_dqd_scores = group_dqd_scores,
    table_dqd_scores = table_dqd_scores,
    table_groups = table_groups,
    num_participants = num_participants,
    total_rows_removed = total_rows_removed,
    type_concept_summary = type_concept_summary,
    has_delivery_data = has_delivery_data,
    has_dqd_data = has_dqd_data
  )

  message("Embedding JavaScript and CSS...")
  # (Already embedded in build function)

  message("Writing HTML file...")
  write_file(html_content, output_path)

  message("\nReport generation complete!")
  message("\nReport Details:")
  message("Site: ", metrics$metadata$site)
  message("Delivery Date: ", metrics$metadata$delivery_date)

  return(invisible(output_path))
}

# ==============================================================================
# HTML BUILDING FUNCTIONS
# ==============================================================================

#' Build complete HTML report with all sections
build_complete_html_report <- function(metrics, dqd_data, dqd_scores, group_dqd_scores,
                                       table_dqd_scores, table_groups, num_participants,
                                       total_rows_removed, type_concept_summary,
                                       has_delivery_data = TRUE, has_dqd_data = TRUE) {

  # Prepare data for embedding
  report_data_json <- prepare_report_data_json(
    metrics, dqd_data, table_groups, group_dqd_scores, table_dqd_scores
  )

  # Build HTML components
  css_styles <- get_full_css_styles()
  sidebar <- build_sidebar(metrics, dqd_scores$overall, has_delivery_data, has_dqd_data)
  header <- build_report_header(metrics, dqd_scores$overall, has_delivery_data, has_dqd_data)
  overview <- build_delivery_overview_section(metrics, dqd_scores, num_participants, total_rows_removed, type_concept_summary, has_delivery_data, has_dqd_data)
  dqd_grid <- build_dqd_grid_section(dqd_scores, has_dqd_data)
  delivery_report <- build_delivery_report_section(metrics, table_groups, group_dqd_scores, has_delivery_data, has_dqd_data)
  drilldown <- build_table_drilldown_section()
  vocab_harm <- build_vocabulary_harmonization_section(metrics, has_delivery_data)
  tech_summary <- build_technical_summary_section(metrics, has_delivery_data)
  javascript <- get_full_javascript()
  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OMOP Data Delivery Report - %s</title>
    <style>%s</style>
</head>
<body>
    <div class="app-wrapper">
        %s
        <div class="main-content">
            %s
            <div class="container">
                %s
                %s
                %s
                %s
                %s
                %s
            </div>
        </div>
    </div>
    <script>
    // Embedded report data
    const REPORT_DATA = %s;
    </script>
    <script>%s</script>
</body>
</html>',
    metrics$metadata$site,
    css_styles,
    sidebar,
    header,
    overview,
    dqd_grid,
    delivery_report,
    drilldown,
    vocab_harm,
    tech_summary,
    report_data_json,
    javascript
  )

  return(html)
}

#' Prepare all report data as JSON for JavaScript access
prepare_report_data_json <- function(metrics, dqd_data, table_groups, group_dqd_scores, table_dqd_scores) {

  # Prepare table summaries for each group
  group_data <- list()

  for (group_name in names(table_groups)) {
    group_tables <- table_groups[[group_name]]

    # Get data for each table in group
    table_data_list <- lapply(group_tables, function(tbl) {
      prepare_table_data(tbl, metrics, table_dqd_scores[[tbl]])
    })
    names(table_data_list) <- group_tables

    # Group-level aggregates
    group_source_vocab <- metrics$source_vocabularies %>%
      dplyr::filter(table_name %in% group_tables) %>%
      dplyr::group_by(vocabulary) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(count))

    group_target_vocab <- metrics$target_vocabularies %>%
      dplyr::filter(table_name %in% group_tables) %>%
      dplyr::group_by(vocabulary) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(count))

    group_transitions <- metrics$table_transitions %>%
      dplyr::filter(source_table %in% group_tables | target_table %in% group_tables)

    # Group-level type concepts (summed across tables in group)
    group_type_concepts_detailed <- metrics$type_concepts_grouped %>%
      dplyr::filter(table_name %in% group_tables) %>%
      dplyr::group_by(type_group, type_concept) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

    # Aggregate by type_group only and ensure all groups present
    group_type_concepts_summary <- group_type_concepts_detailed %>%
      dplyr::group_by(type_group) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      ensure_all_type_groups()

    # Order detailed concepts by canonical group order, then count
    # Note: Don't call ensure_all_type_groups() here because this is detailed data
    # with type_concept column. JavaScript will aggregate and ensure all groups.
    group_type_concepts <- group_type_concepts_detailed %>%
      dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
      dplyr::arrange(type_group, desc(count)) %>%
      dplyr::mutate(type_group = as.character(type_group))

    group_data[[group_name]] <- list(
      tables = table_data_list,
      dqd_score = group_dqd_scores[[group_name]],
      source_vocabularies = group_source_vocab,
      target_vocabularies = group_target_vocab,
      transitions = group_transitions,
      type_concepts = group_type_concepts
    )
  }

  # Add overall transitions for vocabulary harmonization section
  overall_transitions <- metrics$table_transitions

  # Add overall harmonization statuses (summarize across all tables)
  overall_harmonization_statuses <- metrics$harmonization_statuses %>%
    dplyr::group_by(status) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(count))

  # Add overall type concepts (dataset-wide)
  overall_type_concepts_detailed <- metrics$type_concepts_grouped %>%
    dplyr::group_by(type_group, type_concept) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Aggregate by type_group only and ensure all groups present
  overall_type_concepts_summary <- overall_type_concepts_detailed %>%
    dplyr::group_by(type_group) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    ensure_all_type_groups()

  # Order detailed concepts by canonical group order, then count
  # Note: Don't call ensure_all_type_groups() here because this is detailed data
  # with type_concept column. JavaScript will aggregate and ensure all groups.
  overall_type_concepts <- overall_type_concepts_detailed %>%
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
    dplyr::arrange(type_group, desc(count)) %>%
    dplyr::mutate(type_group = as.character(type_group))

  data_list <- list(
    groups = group_data,
    type_colors = as.list(get_type_concept_colors()),
    type_group_order = get_type_concept_group_order(),
    overall_transitions = overall_transitions,
    harmonization_statuses = overall_harmonization_statuses,
    overall_type_concepts = overall_type_concepts
  )

  jsonlite::toJSON(data_list, auto_unbox = TRUE, pretty = FALSE, na = "null")
}

#' Prepare individual table data
prepare_table_data <- function(table_name, metrics, dqd_score) {

  valid_rows <- metrics$valid_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  valid_rows <- ifelse(length(valid_rows) > 0, valid_rows[1], 0)

  invalid_rows <- metrics$invalid_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  invalid_rows <- ifelse(length(invalid_rows) > 0, invalid_rows[1], 0)

  missing_rows <- metrics$missing_person_id %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  missing_rows <- ifelse(length(missing_rows) > 0, missing_rows[1], 0)

  # For person table, use person count instead of row count
  # (since each row in person table = 1 person)
  if (table_name == "person") {
    missing_rows <- metrics$missing_person_id_count
  }

  # Get referential integrity violations for this table
  referential_integrity_violations <- metrics$referential_integrity_violations %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  referential_integrity_violations <- ifelse(length(referential_integrity_violations) > 0, referential_integrity_violations[1], 0)

  # Get final row count from CSV (don't calculate, use the actual value)
  final_rows <- metrics$final_row_counts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(count)
  final_rows <- ifelse(length(final_rows) > 0, final_rows[1], 0)

  type_concepts <- metrics$type_concepts_grouped %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::mutate(type_group = factor(type_group, levels = get_type_concept_group_order())) %>%
    dplyr::arrange(type_group, desc(count)) %>%
    dplyr::mutate(type_group = as.character(type_group))

  invalid_columns <- metrics$invalid_columns %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(column_name)
  # Ensure it's a list/array for JSON
  if (length(invalid_columns) == 0) {
    invalid_columns <- list()
  } else {
    invalid_columns <- as.list(invalid_columns)
  }

  source_vocab <- metrics$source_vocabularies %>%
    dplyr::filter(table_name == !!table_name)

  target_vocab <- metrics$target_vocabularies %>%
    dplyr::filter(table_name == !!table_name)

  transitions <- metrics$table_transitions %>%
    dplyr::filter(source_table == !!table_name | target_table == !!table_name)

  # Get harmonization statuses for this table
  harmonization_statuses <- metrics$harmonization_statuses %>%
    dplyr::filter(table_name == !!table_name)

  # Get row dispositions for this table
  dispositions <- metrics$row_dispositions %>%
    dplyr::filter(table_name == !!table_name)

  # Get default date value counts for this table
  default_date_rows <- metrics$default_date_values %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
    dplyr::pull(total)
  default_date_rows <- ifelse(length(default_date_rows) > 0, default_date_rows[1], 0)

  # Get invalid concept_id counts for this table
  invalid_concept_rows <- metrics$invalid_concepts %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
    dplyr::pull(total)
  invalid_concept_rows <- ifelse(length(invalid_concept_rows) > 0, invalid_concept_rows[1], 0)

  # Get missing columns for this table
  missing_columns_list <- metrics$missing_columns %>%
    dplyr::filter(table_name == !!table_name) %>%
    dplyr::pull(column_name)

  delivered <- table_name %in% metrics$valid_tables$table_name

  initial_rows_calc <- valid_rows + invalid_rows + missing_rows
  quality_issues <- invalid_rows + missing_rows

  # Validation: check if final_rows matches type concept totals
  # Type concepts should sum to final_rows for delivered tables
  type_concept_total <- type_concepts %>%
    dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
    dplyr::pull(total)
  type_concept_total <- ifelse(length(type_concept_total) > 0, type_concept_total[1], 0)

  # Check if table has vocab harmonization transitions
  has_transitions <- nrow(transitions %>% dplyr::filter(target_table == !!table_name | source_table == !!table_name)) > 0

  # Pipeline-derived tables (skip validation for these)
  pipeline_derived_tables <- c("condition_era", "drug_era", "dose_era", "observation_period", "cdm_source")

  # Determine the correct final row count and validate
  if (type_concept_total > 0) {
    # Use type concepts as source of truth
    expected_final <- type_concept_total
    counts_valid <- (type_concept_total == final_rows)
  } else if (valid_rows == 0 && !has_transitions) {
    # Table not delivered and no vocab harmonization - expected final should be 0
    expected_final <- 0
    counts_valid <- (final_rows == 0)
  } else {
    # Otherwise, trust the final_rows artifact
    expected_final <- final_rows
    counts_valid <- TRUE
  }

  # Skip validation warnings for pipeline-derived tables
  if (table_name %in% pipeline_derived_tables) {
    counts_valid <- TRUE
  }

  # Calculate harmonization from the EXPECTED (validated) final rows
  if (valid_rows == 0 && !has_transitions) {
    # Table not delivered and no vocab harmonization - harmonization should be 0
    harmonization <- 0
  } else {
    # Normal case: calculate from expected vs initial
    # This is the TRUE net impact: expected_final = initial_rows - quality_issues + harmonization
    # Therefore: harmonization = expected_final - initial_rows + quality_issues
    harmonization <- expected_final - initial_rows_calc + quality_issues
  }

  list(
    name = table_name,
    delivered = delivered,
    valid_rows = valid_rows,
    invalid_rows = invalid_rows,
    initial_rows = initial_rows_calc,
    final_rows = final_rows,
    missing_person_id_rows = missing_rows,
    referential_integrity_violations = referential_integrity_violations,
    harmonization = harmonization,
    default_date_rows = default_date_rows,
    invalid_concept_rows = invalid_concept_rows,
    missing_columns_added = length(missing_columns_list),
    missing_columns = missing_columns_list,
    dqd_score = dqd_score,
    type_concepts = type_concepts,
    invalid_columns = invalid_columns,
    source_vocabularies = source_vocab,
    target_vocabularies = target_vocab,
    transitions = transitions,
    harmonization_statuses = harmonization_statuses,
    dispositions = dispositions,
    counts_valid = counts_valid,
    expected_final_rows = expected_final
  )
}
