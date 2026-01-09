# ==============================================================================
# Template Data Preparers
# ==============================================================================
# Functions that format data specifically for HTML template rendering.
# These prepare display-ready data structures that match template expectations.

#' Prepare data for overview section template
#'
#' Prepares all variables needed for the overview section template,
#' including formatting and warning indicators.
#'
#' @param metrics List of metrics from parse_delivery_metrics()
#' @param dqd_scores List with DQD scores
#' @param num_participants Integer participant count
#' @param total_rows_removed Integer rows removed count
#' @param has_delivery_data Logical whether delivery data is available
#' @param has_dqd_data Logical whether DQD data is available
#' @return List of template variables
#' @export
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

#' Prepare DQD grid rows data for template
#'
#' Transforms DQD grid data into row structures for template rendering.
#'
#' @param grid Data frame with DQD grid data
#' @return List of row data for rendering
#' @export
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

#' Prepare data for time series section template
#'
#' Calculates year ranges and formats time series data for template rendering.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @return List with template variables or NULL if no data
#' @export
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

#' Prepare data for vocabulary harmonization section template
#'
#' Aggregates source and target vocabularies for top 10 display.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @return List with source and target vocabulary data
#' @export
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

#' Prepare data for delivery report section template
#'
#' Formats table data with all display formatting, warnings, and icons.
#'
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @param table_groups Named list of table groups
#' @param group_dqd_scores Named list of DQD scores per group
#' @param num_participants Integer number of participants
#' @return List with dropdown options and group contents data
#' @export
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

#' Prepare formatted data for a single table row in delivery report template
#'
#' Formats all fields for a single table row with appropriate styling classes.
#'
#' @param table_name Character table name
#' @param metrics List of parsed metrics from parse_delivery_metrics()
#' @param num_participants Integer number of participants
#' @return List with all formatted fields for template
#' @export
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
