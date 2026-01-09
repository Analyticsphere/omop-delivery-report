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

  # Prepare report data (all business logic calculations)
  report_data <- prepare_report_data(metrics, table_groups, group_dqd_scores, table_dqd_scores)

  # Serialize to JSON for JavaScript
  report_data_json <- build_report_data_json(report_data)

  # Build sections
  sidebar_html <- render_template("sections/sidebar", list(
    site_name = if (has_delivery_data) metrics$metadata$site else "Unknown Site"
  ))

  header_html <- render_template("sections/header", list(
    site_name = if (has_delivery_data) metrics$metadata$site else "Unknown Site",
    delivery_date = if (has_delivery_data) metrics$metadata$delivery_date else "Unknown"
  ))

  # Overview section - use template
  overview_html <- if (!has_delivery_data && !has_dqd_data) {
    render_template("sections/data-unavailable", list(
      section_id = "overview",
      section_title = "Delivery Overview",
      data_type = "Delivery and DQD"
    ))
  } else {
    overview_data <- prepare_overview_data(metrics, dqd_scores, num_participants, total_rows_removed, has_delivery_data, has_dqd_data)
    render_template("sections/overview", overview_data)
  }

  # DQD grid section - use template
  dqd_grid_html <- if (!has_dqd_data) {
    render_template("sections/data-unavailable", list(
      section_id = "dqd-grid",
      section_title = "Data Quality Dashboard Results",
      data_type = "DQD"
    ))
  } else {
    grid_rows_data <- prepare_dqd_grid_rows(dqd_scores$grid)
    grid_rows_html <- if (length(grid_rows_data) > 0) {
      render_component_list("components/dqd-grid-row", grid_rows_data)
    } else {
      '<tr><td colspan="13">No DQD data available</td></tr>'
    }
    render_template("sections/dqd-grid", list(grid_rows = grid_rows_html))
  }

  # Time series section - use template
  time_series_html <- if (!has_delivery_data) {
    render_template("sections/data-unavailable", list(
      section_id = "time-series",
      section_title = "Data Timeline",
      data_type = "Delivery"
    ))
  } else {
    ts_data <- prepare_time_series_data(metrics)
    if (is.null(ts_data)) {
      render_template("sections/time-series-empty")
    } else {
      render_template("sections/time-series", ts_data)
    }
  }

  # Delivery report section - use templates
  delivery_report_html <- if (!has_delivery_data) {
    render_template("sections/data-unavailable", list(
      section_id = "delivery-report",
      section_title = "Table Delivery Summary",
      data_type = "Delivery"
    ))
  } else {
    dr_data <- prepare_delivery_report_data(metrics, table_groups, group_dqd_scores, num_participants)

    # Render dropdown options
    dropdown_options_html <- render_component_list("components/delivery-report-dropdown-option", dr_data$dropdown_options_data)

    # Render each group
    group_contents_html <- paste(sapply(dr_data$group_contents_data, function(group_data) {
      # Render table rows for this group
      table_rows_html <- render_component_list("components/delivery-report-table-row", group_data$table_rows_data)

      # Render the group with its rows
      render_template("components/delivery-report-group", c(
        group_data[c("group_name", "group_id", "display_style", "dqd_note", "type_concept_subheader")],
        list(table_rows = table_rows_html)
      ))
    }), collapse = "\n")

    # Render the main section
    render_template("sections/delivery-report", list(
      dropdown_options = dropdown_options_html,
      group_contents = group_contents_html
    ))
  }

  # Vocabulary harmonization section - use template
  vocab_harm_html <- if (!has_delivery_data) {
    render_template("sections/data-unavailable", list(
      section_id = "vocab-harmonization",
      section_title = "Vocabulary Harmonization",
      data_type = "Delivery"
    ))
  } else {
    vh_data <- prepare_vocab_harmonization_data(metrics)

    # Render vocabulary rows
    source_vocab_rows_html <- if (length(vh_data$source_vocab_rows) > 0) {
      render_component_list("components/vocab-row", vh_data$source_vocab_rows)
    } else {
      '<tr><td colspan="2">No vocabulary data available</td></tr>'
    }

    target_vocab_rows_html <- if (length(vh_data$target_vocab_rows) > 0) {
      render_component_list("components/vocab-row", vh_data$target_vocab_rows)
    } else {
      '<tr><td colspan="2">No vocabulary data available</td></tr>'
    }

    # Render the section
    render_template("sections/vocabulary-harmonization", list(
      delivered_vocab_version = vh_data$delivered_vocab_version,
      standardized_vocab_version = vh_data$standardized_vocab_version,
      source_vocab_rows = source_vocab_rows_html,
      target_vocab_rows = target_vocab_rows_html
    ))
  }

  drilldown_html <- render_template("sections/drilldown")

  technical_html <- render_template("sections/technical", list(
    processing_date = if (has_delivery_data) metrics$metadata$processing_date else "Unknown",
    delivered_cdm_version = if (has_delivery_data) metrics$metadata$delivered_cdm_version else "Unknown",
    standardized_cdm_version = if (has_delivery_data) metrics$metadata$standardized_cdm_version else "Unknown",
    file_format = if (has_delivery_data) toupper(metrics$metadata$file_format) else "UNKNOWN",
    pipeline_version = if (has_delivery_data) metrics$metadata$pipeline_version else "Unknown"
  ))

  # Assemble complete HTML using main template
  html <- render_template("main", list(
    site_name = if (has_delivery_data) metrics$metadata$site else "Unknown Site",
    css_styles = get_full_css_styles(),
    sidebar_html = sidebar_html,
    header_html = header_html,
    overview_html = overview_html,
    dqd_grid_html = dqd_grid_html,
    time_series_html = time_series_html,
    delivery_report_html = delivery_report_html,
    vocab_harm_html = vocab_harm_html,
    drilldown_html = drilldown_html,
    technical_html = technical_html,
    report_data_json = report_data_json,
    javascript = get_full_javascript()
  ))

  return(html)
}