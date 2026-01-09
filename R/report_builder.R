# ==============================================================================
# Report Builder
# ==============================================================================
# Assembles the final HTML report from templates and data.
# This is the orchestration layer that brings everything together.

#' Build the complete HTML report
#'
#' Main entry point for report generation. Assembles all sections
#' using templates and data.
#'
#' @param metrics List of parsed delivery metrics
#' @param dqd_data Data frame with DQD results (or NULL)
#' @param dqd_scores List with overall score and grid
#' @param table_groups Named list of table groups
#' @param group_dqd_scores Named list of DQD scores per group
#' @param table_dqd_scores Named list of DQD scores per table
#' @param has_delivery_data Logical whether delivery data available
#' @param has_dqd_data Logical whether DQD data available
#' @return Character string containing complete HTML report
build_complete_html_report <- function(metrics, dqd_data, dqd_scores, table_groups,
                                       group_dqd_scores, table_dqd_scores,
                                       has_delivery_data, has_dqd_data) {

  # Calculate summary metrics
  num_participants <- if (has_delivery_data) calculate_num_participants(metrics) else 0
  total_rows_removed <- if (has_delivery_data) calculate_total_rows_removed(metrics) else 0

  # Build JSON data for JavaScript
  report_data_json <- build_report_data_json(metrics, dqd_data, table_groups, group_dqd_scores, table_dqd_scores)

  # Build sections
  sidebar_html <- render_template("sections/sidebar", list(
    site_name = if (has_delivery_data) metrics$metadata$site else "Unknown Site"
  ))

  header_html <- render_template("sections/header", list(
    site_name = if (has_delivery_data) metrics$metadata$site else "Unknown Site",
    delivery_date = if (has_delivery_data) metrics$metadata$delivery_date else "Unknown"
  ))

  overview_html <- build_overview_section(metrics, dqd_scores, num_participants, total_rows_removed, has_delivery_data, has_dqd_data)

  dqd_grid_html <- build_dqd_grid_section(dqd_scores, has_dqd_data)

  time_series_html <- build_time_series_section(metrics, has_delivery_data)

  delivery_report_html <- build_delivery_report_section(metrics, table_groups, group_dqd_scores, has_delivery_data, has_dqd_data)

  vocab_harm_html <- build_vocabulary_harmonization_section(metrics, has_delivery_data)

  drilldown_html <- render_template("sections/drilldown")

  technical_html <- render_template("sections/technical", list(
    processing_date = if (has_delivery_data) metrics$metadata$processing_date else "Unknown",
    delivered_cdm_version = if (has_delivery_data) metrics$metadata$delivered_cdm_version else "Unknown",
    standardized_cdm_version = if (has_delivery_data) metrics$metadata$standardized_cdm_version else "Unknown",
    file_format = if (has_delivery_data) toupper(metrics$metadata$file_format) else "UNKNOWN",
    pipeline_version = if (has_delivery_data) metrics$metadata$pipeline_version else "Unknown"
  ))

  # Assemble complete HTML
  html <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OMOP Delivery Report - %s</title>
    <style>
%s
    </style>
</head>
<body>
    %s
    <div class="main-content">
        %s
        %s
        %s
        %s
        %s
        %s
        %s
        %s
    </div>

    <script>
    // Report data for JavaScript
    const REPORT_DATA = %s;
    </script>
    <script>
%s
    </script>
</body>
</html>',
    if (has_delivery_data) metrics$metadata$site else "Unknown Site",
    get_full_css_styles(),
    sidebar_html,
    header_html,
    overview_html,
    dqd_grid_html,
    time_series_html,
    delivery_report_html,
    vocab_harm_html,
    drilldown_html,
    technical_html,
    report_data_json,
    get_full_javascript()
  )

  return(html)
}

#' Build overview section
#'
#' Kept as R function due to complex conditional logic.
#'
#' @param metrics List of metrics
#' @param dqd_scores List with DQD scores
#' @param num_participants Integer participant count
#' @param total_rows_removed Integer rows removed count
#' @param has_delivery_data Logical
#' @param has_dqd_data Logical
#' @return Character HTML string
build_overview_section <- function(metrics, dqd_scores, num_participants, total_rows_removed, has_delivery_data, has_dqd_data) {
  # Use template for structure, but this has complex conditional rendering
  # For now, keep as function (could be further templateized later)

  if (!has_delivery_data && !has_dqd_data) {
    return('<div class="section" id="overview"><div class="info-box data-unavailable"><h4>⚠️ Data Not Available</h4><p><strong>Delivery and DQD data files were not found.</strong></p></div></div>')
  }

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

  sprintf('
    <div class="section" id="overview">
        <div class="section-header">
            <span class="section-icon"></span>
            <h2>Delivery Overview</h2>
        </div>

        <div class="overview-metrics-grid">
            <div class="metric-card">
                <div class="metric-label">Tables Delivered</div>
                <div class="metric-value">%s</div>
                <div class="metric-sublabel">OMOP CDM tables</div>
            </div>

            <div class="metric-card">
                <div class="metric-label">Participants</div>
                <div class="metric-value">%s</div>
                <div class="metric-sublabel">Total identifiable persons in dataset</div>
            </div>

            <div class="metric-card">
                <div class="metric-label">Data Quality Score</div>
                <div class="text-center mt-20">
                    <div class="dqd-score %s">%s</div>
                </div>
            </div>

            <div class="metric-card%s">
                <div class="metric-label">Missing Identifiers</div>
                <div class="metric-value">%s %s</div>
                <div class="metric-sublabel">%s with missing Connect ID</div>
            </div>

            <div class="metric-card%s">
                <div class="metric-label">Rows Removed</div>
                <div class="metric-value">%s %s</div>
                <div class="metric-sublabel">Due to missing Connect ID</div>
            </div>
        </div>

        <div class="overview-section-title">
            <div class="chart-title overview-section-subtitle">Type Concept Breakdown</div>
            <div class="overview-disclaimer">Dataset-wide</div>
            <div class="chart-container overview-metrics-spacer">
                <div id="dataset-type-concepts" class="type-concept-chart">
                    <!-- Dynamically populated by JavaScript -->
                </div>
            </div>
        </div>
    </div>',
    tables_delivered,
    participants_display,
    dqd_class, dqd_score_display,
    missing_warning, missing_icon, missing_person_display, person_word,
    rows_warning, rows_icon, rows_removed_display
  )
}

#' Build DQD grid section
#'
#' Generates the DQD results grid table.
#'
#' @param dqd_scores List with DQD scores and grid
#' @param has_dqd_data Logical
#' @return Character HTML string
build_dqd_grid_section <- function(dqd_scores, has_dqd_data) {
  if (!has_dqd_data) {
    return('<div class="section" id="dqd-grid"><div class="info-box data-unavailable"><h4>⚠️ Data Not Available</h4><p><strong>DQD data file was not found.</strong></p></div></div>')
  }

  # Generate table rows from grid
  grid_rows <- generate_dqd_grid_rows(dqd_scores$grid)

  sprintf('
    <div class="section" id="dqd-grid">
        <div class="section-header">
            <span class="section-icon"></span>
            <h2>Data Quality Dashboard Results</h2>
        </div>

        <div class="table-container">
            <table class="dqd-grid-table">
                <thead>
                    <tr>
                        <th rowspan="2">Category</th>
                        <th colspan="4">Verification</th>
                        <th colspan="4">Validation</th>
                        <th colspan="4">Total</th>
                    </tr>
                    <tr>
                        <th>Pass</th><th>Fail</th><th>Total</th><th>%% Pass</th>
                        <th>Pass</th><th>Fail</th><th>Total</th><th>%% Pass</th>
                        <th>Pass</th><th>Fail</th><th>Total</th><th>%% Pass</th>
                    </tr>
                </thead>
                <tbody>
                    %s
                </tbody>
            </table>
        </div>
    </div>',
    grid_rows
  )
}

#' Generate DQD grid table rows
#'
#' @param grid Data frame with DQD grid data
#' @return Character HTML string with table rows
generate_dqd_grid_rows <- function(grid) {
  if (nrow(grid) == 0) return('<tr><td colspan="13">No DQD data available</td></tr>')

  # Reshape for display
  grid_wide <- grid |>
    tidyr::pivot_wider(names_from = context, values_from = c(Pass, Fail, Total, `% Pass`))

  categories <- c("Plausibility", "Conformance", "Completeness", "Total")

  rows <- sapply(categories, function(cat_name) {
    row_data <- grid_wide |> dplyr::filter(category == cat_name)

    if (nrow(row_data) == 0) return("")

    row_class <- if (cat_name == "Total") ' class="total-row"' else ''

    sprintf('
        <tr%s>
            <td><strong>%s</strong></td>
            <td class="pass-cell">%s</td>
            <td class="fail-cell">%s</td>
            <td>%s</td>
            <td><strong>%s</strong></td>
            <td class="pass-cell">%s</td>
            <td class="fail-cell">%s</td>
            <td>%s</td>
            <td><strong>%s</strong></td>
            <td class="pass-cell">%s</td>
            <td class="fail-cell">%s</td>
            <td>%s</td>
            <td><strong>%s%%</strong></td>
        </tr>',
      row_class,
      cat_name,
      ifelse(is.na(row_data$Pass_Verification), 0, row_data$Pass_Verification),
      ifelse(is.na(row_data$Fail_Verification), 0, row_data$Fail_Verification),
      ifelse(is.na(row_data$Total_Verification), 0, row_data$Total_Verification),
      ifelse(is.na(row_data$`% Pass_Verification`), "0%", paste0(row_data$`% Pass_Verification`, "%")),
      ifelse(is.na(row_data$Pass_Validation), 0, row_data$Pass_Validation),
      ifelse(is.na(row_data$Fail_Validation), 0, row_data$Fail_Validation),
      ifelse(is.na(row_data$Total_Validation), 0, row_data$Total_Validation),
      ifelse(is.na(row_data$`% Pass_Validation`), "0%", paste0(row_data$`% Pass_Validation`, "%")),
      ifelse(is.na(row_data$Pass_Total), 0, row_data$Pass_Total),
      ifelse(is.na(row_data$Fail_Total), 0, row_data$Fail_Total),
      ifelse(is.na(row_data$Total_Total), 0, row_data$Total_Total),
      ifelse(is.na(row_data$`% Pass_Total`), 0, row_data$`% Pass_Total`)
    )
  })

  paste(rows, collapse = "\n")
}

# Note: Time series, delivery report, and vocab harmonization sections
# use html_builders.R functions directly for building complex HTML structures.
