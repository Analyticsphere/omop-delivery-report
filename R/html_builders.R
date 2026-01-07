# ==============================================================================
# HTML Section Builders
# ==============================================================================

#' Build data unavailable message box
#'
#' @param data_type Character. Type of data that's unavailable (e.g., "DQD", "Delivery")
#' @param section_title Character. Title of the section
#' @return HTML string with warning message
build_data_unavailable_message <- function(data_type, section_title) {
  sprintf('
    <div class="section" id="%s">
      <div class="section-header">
        <h2>%s</h2>
      </div>
      <div class="info-box data-unavailable">
        <h4>⚠️ Data Not Available</h4>
        <p><strong>%s data file was not found.</strong></p>
        <p>This section cannot be displayed without the required input data.</p>
      </div>
    </div>',
    tolower(gsub(" ", "-", section_title)),
    section_title,
    data_type
  )
}

#' Build sidebar navigation
build_sidebar <- function(metrics, dqd_score, has_delivery_data = TRUE, has_dqd_data = TRUE) {
  sprintf('
    <div class="sidebar">
        <div class="sidebar-header">
            <h1>OMOP REPORT</h1>
            <div class="sidebar-subtitle">%s</div>
        </div>
        <nav class="sidebar-nav">
            <a href="#overview" class="sidebar-nav-item" onclick="scrollToSection(event, \'overview\')">Overview</a>
            <a href="#dqd-grid" class="sidebar-nav-item" onclick="scrollToSection(event, \'dqd-grid\')">DQD Results</a>
            <a href="#time-series" class="sidebar-nav-item" onclick="scrollToSection(event, \'time-series\')">Data Timeline</a>
            <a href="#delivery-report" class="sidebar-nav-item" onclick="scrollToSection(event, \'delivery-report\')">Tables</a>
            <a href="#vocab-harmonization" class="sidebar-nav-item" onclick="scrollToSection(event, \'vocab-harmonization\')">Vocabularies</a>
            <a href="#technical-summary" class="sidebar-nav-item" onclick="scrollToSection(event, \'technical-summary\')">Technical</a>
        </nav>
    </div>',
    metrics$metadata$site
  )
}

#' Build report header
build_report_header <- function(metrics, dqd_score, has_delivery_data = TRUE, has_dqd_data = TRUE) {
  site_name <- if (has_delivery_data) metrics$metadata$site else "Unknown Site"
  delivery_date <- if (has_delivery_data) metrics$metadata$delivery_date else "Unknown"

  sprintf('
    <div class="report-header">
        <div class="report-header-content">
            <div>
                <h1>%s</h1>
                <div class="report-subtitle" style="font-size: 1.2em; font-weight: 500; color: #64748b; margin-top: 8px;">Delivery Date: %s</div>
            </div>
        </div>
    </div>',
    site_name,
    delivery_date
  )
}

#' Build delivery overview section
build_delivery_overview_section <- function(metrics, dqd_scores, num_participants,
                                            total_rows_removed, type_concept_summary,
                                            has_delivery_data = TRUE, has_dqd_data = TRUE) {

  # Handle case where neither data source is available
  if (!has_delivery_data && !has_dqd_data) {
    return(build_data_unavailable_message("Delivery and DQD", "Delivery Overview"))
  }

  # Format delivery metrics (N/A when data unavailable)
  if (has_delivery_data) {
    tables_delivered_display <- as.character(nrow(metrics$valid_tables))
    participants_display <- format(num_participants, big.mark = ",")
    missing_person_display <- as.character(metrics$missing_person_id_count)
    rows_removed_display <- format(total_rows_removed, big.mark = ",")

    # Warnings
    missing_warning <- ifelse(metrics$missing_person_id_count > 0, " warning", " success")
    missing_icon <- ifelse(metrics$missing_person_id_count > 0,
                           '<span class="warning-icon">⚠️</span>',
                           '<span class="success-icon">✓</span>')

    rows_warning <- ifelse(total_rows_removed > 0, " warning", " success")
    rows_icon <- ifelse(total_rows_removed > 0,
                        '<span class="warning-icon">⚠️</span>',
                        '<span class="success-icon">✓</span>')

    # Pluralization
    person_word <- ifelse(metrics$missing_person_id_count == 1, "Person", "Persons")
  } else {
    # Show N/A when delivery data unavailable
    tables_delivered_display <- "N/A"
    participants_display <- "N/A"
    missing_person_display <- "N/A"
    rows_removed_display <- "N/A"

    # No warnings when data unavailable
    missing_warning <- " neutral"
    missing_icon <- ""
    rows_warning <- " neutral"
    rows_icon <- ""
    person_word <- "Persons"
  }

  # DQD score class (handle NA when DQD data unavailable)
  dqd_class <- if (is.na(dqd_scores$overall)) {
    "neutral"
  } else if (dqd_scores$overall >= 95) {
    "good"
  } else if (dqd_scores$overall >= 85) {
    "fair"
  } else {
    "poor"
  }

  # DQD score display
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

        <div style="margin-top: 32px;">
            <div class="chart-title" style="margin-bottom: 4px;">Type Concept Breakdown</div>
            <div style="font-size: 0.9em; color: #94a3b8; margin-bottom: 8px; text-align: center;">Dataset-wide</div>
            <div class="chart-container" style="margin-top: 16px;">
                <div id="dataset-type-concepts" class="type-concept-chart">
                    <!-- Dynamically populated by JavaScript -->
                </div>
            </div>
        </div>
    </div>',
    tables_delivered_display,
    participants_display,
    dqd_class, dqd_score_display,
    missing_warning, missing_icon, missing_person_display, person_word,
    rows_warning, rows_icon, rows_removed_display
  )
}

#' Build delivery report section with table groups
build_delivery_report_section <- function(metrics, table_groups, group_dqd_scores, has_delivery_data = TRUE, has_dqd_data = TRUE) {

  # Return unavailable message if delivery data is missing
  if (!has_delivery_data) {
    return(build_data_unavailable_message("Delivery", "Table Delivery Summary"))
  }

  # Build dropdown options
  dropdown_options <- paste(sapply(names(table_groups), function(group_name) {
    selected <- if (group_name == "Clinical Data") ' selected' else ''
    sprintf('<option value="%s"%s>%s</option>',
            group_name, selected, group_name)
  }), collapse = "\n")

  # Build content for each group
  group_contents <- paste(sapply(names(table_groups), function(group_name) {
    build_table_group_content(group_name, table_groups[[group_name]], metrics, group_dqd_scores[[group_name]])
  }), collapse = "\n")

  sprintf('
    <div class="section" id="delivery-report">
        <div class="section-header">
            <span class="section-icon">📋</span>
            <h2>Delivery Report</h2>
        </div>

        <div class="controls">
            <div class="control-group">
                <label class="control-label" for="table-group-selector">Select Table Group:</label>
                <select id="table-group-selector" onchange="switchTableGroup(this.value)">
                    %s
                </select>
                <button onclick="exportTableToCSV()" class="export-button" style="margin-left: 20px;">
                    Export Counts to CSV
                </button>
            </div>
        </div>

        %s
    </div>',
    dropdown_options,
    group_contents
  )
}

#' Build content for a single table group
build_table_group_content <- function(group_name, group_tables, metrics, group_dqd_score) {

  display_style <- if (group_name == "Clinical Data") "" else "display: none;"
  group_id <- gsub(" ", "-", tolower(group_name))

  # Calculate number of participants
  num_participants <- sum(
    ifelse(nrow(metrics$valid_row_counts %>% dplyr::filter(table_name == "person")) > 0,
           metrics$valid_row_counts %>% dplyr::filter(table_name == "person") %>% dplyr::pull(count), 0),
    ifelse(nrow(metrics$invalid_row_counts %>% dplyr::filter(table_name == "person")) > 0,
           metrics$invalid_row_counts %>% dplyr::filter(table_name == "person") %>% dplyr::pull(count), 0)
  )

  # Build table rows
  table_rows <- paste(sapply(group_tables, function(tbl) {
    valid_rows <- metrics$valid_row_counts %>% dplyr::filter(table_name == tbl) %>% dplyr::pull(count)
    valid_rows <- ifelse(length(valid_rows) > 0, valid_rows[1], 0)

    invalid_rows <- metrics$invalid_row_counts %>% dplyr::filter(table_name == tbl) %>% dplyr::pull(count)
    invalid_rows <- ifelse(length(invalid_rows) > 0, invalid_rows[1], 0)

    final_rows <- metrics$final_row_counts %>% dplyr::filter(table_name == tbl) %>% dplyr::pull(count)
    final_rows <- ifelse(length(final_rows) > 0, final_rows[1], 0)

    missing_rows <- metrics$missing_person_id %>% dplyr::filter(table_name == tbl) %>% dplyr::pull(count)
    missing_rows <- ifelse(length(missing_rows) > 0, missing_rows[1],
                           # For person table, use the person count if no specific entry exists
                           ifelse(tbl == "person", metrics$missing_person_id_count, 0))

    initial_rows <- valid_rows + invalid_rows + missing_rows

    # Calculate Quality Issues (combined)
    quality_issues <- invalid_rows + missing_rows

    # Get type concepts for validation
    type_concept_total <- metrics$type_concepts_grouped %>%
      dplyr::filter(table_name == tbl) %>%
      dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
      dplyr::pull(total)
    type_concept_total <- ifelse(length(type_concept_total) > 0, type_concept_total[1], 0)

    # Determine the expected final count
    # If we have type concepts, use them as the source of truth
    # Otherwise, trust the final_rows artifact
    expected_final <- if (type_concept_total > 0) type_concept_total else final_rows

    # Calculate Harmonization from the EXPECTED (validated) final rows
    # Special case: If table not delivered (valid_rows = 0) and no actual vocab harmonization occurred,
    # harmonization should be 0 (not calculated from row difference which may be a data artifact)
    has_transitions <- nrow(metrics$table_transitions %>%
                              dplyr::filter(target_table == tbl | source_table == tbl)) > 0

    if (valid_rows == 0 && !has_transitions) {
      # Table not delivered and no vocab harmonization - harmonization should be 0
      harmonization <- 0
    } else {
      # Normal case: calculate from expected vs initial
      # This is the TRUE net impact: expected_final = initial_rows - quality_issues + harmonization
      # Therefore: harmonization = expected_final - initial_rows + quality_issues
      harmonization <- expected_final - initial_rows + quality_issues
    }

    # Format Harmonization with sign
    harmonization_display <- if (harmonization > 0) {
      paste0("+", format(harmonization, big.mark = ","))
    } else if (harmonization < 0) {
      format(harmonization, big.mark = ",")
    } else {
      "0"
    }

    # Determine harmonization CSS class
    harmonization_class <- if (harmonization > 0) {
      "harmonization-positive"
    } else if (harmonization < 0) {
      "harmonization-negative"
    } else {
      "harmonization-neutral"
    }

    # Format Quality Issues with minus sign and CSS class
    quality_issues_display <- if (quality_issues > 0) {
      paste0("-", format(quality_issues, big.mark = ","))
    } else {
      format(quality_issues, big.mark = ",")
    }

    quality_issues_class <- if (quality_issues > 0) {
      "harmonization-negative"  # Reuse existing red color class
    } else {
      "harmonization-neutral"
    }

    # Calculate row-per-patient
    row_per_patient <- if (num_participants > 0 && final_rows > 0) {
      sprintf("%.2f", final_rows / num_participants)
    } else {
      "0.00"
    }

    delivered <- tbl %in% metrics$valid_tables$table_name
    status_icon <- if (delivered) "Delivered" else "Not Delivered"
    status_class <- if (delivered) "delivered" else "not-delivered"

    # Validate row counts: initial - quality_issues + harmonization = final
    # Skip validation for pipeline-derived tables
    pipeline_derived_tables <- c("condition_era", "drug_era", "dose_era", "observation_period", "cdm_source")
    expected_final <- initial_rows - quality_issues + harmonization
    counts_valid <- (expected_final == final_rows)

    # Only show warning if counts don't match AND table is not pipeline-derived
    show_count_warning <- !counts_valid && !(tbl %in% pipeline_derived_tables)
    warning_icon <- if (show_count_warning) " 🧮" else ""

    # Quality metric warnings: default dates and invalid concepts
    # Skip default date warnings for vocabulary tables (the dates in vocab tables may not be parsed correctly, but don't impact data quality)
    vocabulary_tables <- c(
      "concept", "vocabulary", "domain", "concept_class",
      "concept_relationship", "relationship", "concept_synonym",
      "concept_ancestor", "source_to_concept_map", "drug_strength"
    )

    default_date_count <- metrics$default_date_values %>%
      dplyr::filter(table_name == tbl) %>%
      dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
      dplyr::pull(total)
    default_date_count <- ifelse(length(default_date_count) > 0, default_date_count[1], 0)

    invalid_concept_count <- metrics$invalid_concepts %>%
      dplyr::filter(table_name == tbl) %>%
      dplyr::summarise(total = sum(count, na.rm = TRUE)) %>%
      dplyr::pull(total)
    invalid_concept_count <- ifelse(length(invalid_concept_count) > 0, invalid_concept_count[1], 0)

    # Get referential integrity violations count
    referential_integrity_count <- metrics$referential_integrity_violations %>%
      dplyr::filter(table_name == tbl) %>%
      dplyr::pull(count)
    referential_integrity_count <- ifelse(length(referential_integrity_count) > 0, referential_integrity_count[1], 0)

    # Calculate percentages and determine if warnings should show
    default_date_pct <- if (final_rows > 0) (default_date_count / final_rows) * 100 else 0
    invalid_concept_pct <- if (final_rows > 0) (invalid_concept_count / final_rows) * 100 else 0

    # Skip default date warnings for vocabulary tables
    show_default_date_warning <- default_date_pct > 1 && !(tbl %in% vocabulary_tables)
    show_invalid_concept_warning <- invalid_concept_count > 0
    show_invalid_rows_warning <- invalid_rows > 0
    show_missing_connect_id_warning <- missing_rows > 0
    show_referential_integrity_warning <- referential_integrity_count > 0

    default_date_warning <- if (show_default_date_warning) " 📅" else ""
    invalid_concept_warning <- if (show_invalid_concept_warning) " 📖" else ""
    invalid_rows_warning <- if (show_invalid_rows_warning) " 🧩" else ""
    missing_connect_id_warning <- if (show_missing_connect_id_warning) " 👤" else ""
    referential_integrity_warning <- if (show_referential_integrity_warning) " 🧑‍🧒" else ""

    # Combine all warning icons
    all_warnings <- paste0(warning_icon, default_date_warning, invalid_concept_warning, invalid_rows_warning, missing_connect_id_warning, referential_integrity_warning)

    # Highlight row if ANY data quality alert exists
    has_any_alert <- show_count_warning || show_default_date_warning || show_invalid_concept_warning || show_invalid_rows_warning || show_missing_connect_id_warning || show_referential_integrity_warning
    row_class <- if (has_any_alert) "row-warning" else ""

    sprintf('
        <tr class="clickable %s" onclick="showTableDrilldown(\'%s\')">
            <td><strong>%s%s</strong></td>
            <td><span class="status-badge status-%s">%s</span></td>
            <td>%s</td>
            <td><span class="%s">%s</span></td>
            <td><span class="%s">%s</span></td>
            <td>%s</td>
            <td>%s</td>
        </tr>',
      row_class,
      tbl,
      tbl,
      all_warnings,
      status_class,
      status_icon,
      format(initial_rows, big.mark = ","),
      quality_issues_class,
      quality_issues_display,
      harmonization_class,
      harmonization_display,
      format(final_rows, big.mark = ","),
      row_per_patient
    )
  }), collapse = "\n")

  dqd_note <- if (!is.na(group_dqd_score)) {
    sprintf('<p style="margin: 0 0 4px 0;"><strong>Data Quality Score for this group:</strong> <span class="dqd-inline">%s%%</span></p>',
            group_dqd_score)
  } else {
    '<p style="margin: 0 0 4px 0;"><strong>Data Quality Score:</strong> <span class="text-muted">Not available</span></p>'
  }

  # Dynamic Type Concept Breakdown subheader based on group name
  type_concept_subheader <- if (group_name == "All Tables") {
    "All Tables"
  } else {
    sprintf("%s Tables", group_name)
  }

  sprintf('
    <div class="table-group-content" id="group-%s" style="%s">
        <h3 style="margin: 0 0 8px 0; font-size: 1.3em; color: #0f172a;">%s</h3>

        %s

        <h4 style="margin-top: 12px; margin-bottom: 8px; font-size: 1.1em; color: #0f172a; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;">Table Delivery Summary</h4>
        <div class="table-container">
            <table id="delivery-table-%s">
                <thead>
                    <tr>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 0, \'text\')">Table Name <span class="sort-indicator"></span></th>
                        <th>Status</th>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 2, \'number\')">Initial Rows <span class="sort-indicator"></span></th>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 3, \'number\')">Quality Issues <span class="sort-indicator"></span></th>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 4, \'number\')">Harmonization <span class="sort-indicator"></span></th>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 5, \'number\')">Final Rows <span class="sort-indicator"></span></th>
                        <th class="sortable" onclick="sortDeliveryTable(\'%s\', 6, \'number\')">Row-Per-Patient <span class="sort-indicator"></span></th>
                    </tr>
                </thead>
                <tbody id="delivery-tbody-%s">
                    %s
                </tbody>
            </table>
        </div>

        <div class="subsection">
            <h4 style="margin-bottom: 4px;">Type Concept Breakdown</h4>
            <div style="font-size: 0.9em; color: #94a3b8; margin-bottom: 8px; text-align: center;">%s</div>
            <div id="group-type-concepts-%s" class="chart-container" style="margin-top: 8px;">
                <!-- Dynamically populated by JavaScript -->
            </div>
        </div>
    </div>',
    group_id, display_style,
    group_name,
    dqd_note,
    group_id, # table ID
    group_id, group_id, group_id, group_id, group_id, group_id, # onclick handlers (6 sortable columns - Status is not sortable)
    group_id, # tbody ID
    table_rows,
    type_concept_subheader, # dynamic subheader for Type Concept Breakdown
    group_id # type concepts div ID
  )
}

#' Build time series section
build_time_series_section <- function(metrics, has_delivery_data = TRUE) {

  # Return unavailable message if delivery data is missing
  if (!has_delivery_data) {
    return(build_data_unavailable_message("Delivery", "Data Timeline"))
  }

  # Check if time series data exists
  if (nrow(metrics$time_series) == 0) {
    return(sprintf('
    <div class="section" id="time-series">
        <div class="section-header">
            <span class="section-icon"></span>
            <h2>Data Timeline</h2>
        </div>
        <p class="text-muted">No time series data available</p>
    </div>'))
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

  sprintf('
    <div class="section" id="time-series">
        <div class="section-header">
            <span class="section-icon"></span>
            <h2>Data Timeline</h2>
        </div>

        <div style="margin-bottom: 24px;">
            <div class="toggle-buttons">
                <button id="btn-recent-view" class="toggle-button active" onclick="switchTimeSeriesView(\'recent\')">
                    Last 15 Years (%d\u2013%d)
                </button>
                <button id="btn-custom-view" class="toggle-button" onclick="switchTimeSeriesView(\'custom\')">
                    Custom
                </button>
            </div>

            <div id="custom-year-controls" style="display: none; margin-top: 16px; padding: 16px; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0;">
                <div style="display: flex; align-items: center; gap: 16px; flex-wrap: wrap;">
                    <div style="display: flex; align-items: center; gap: 8px;">
                        <label for="custom-start-year" style="font-weight: 500; color: #475569; font-size: 0.9em;">From:</label>
                        <input type="number" id="custom-start-year" value="%d" min="1900" max="2100"
                               style="width: 80px; padding: 6px 10px; border: 1px solid #cbd5e1; border-radius: 6px; font-size: 0.9em;">
                    </div>
                    <div style="display: flex; align-items: center; gap: 8px;">
                        <label for="custom-end-year" style="font-weight: 500; color: #475569; font-size: 0.9em;">To:</label>
                        <input type="number" id="custom-end-year" value="%d" min="1900" max="2100"
                               style="width: 80px; padding: 6px 10px; border: 1px solid #cbd5e1; border-radius: 6px; font-size: 0.9em;">
                    </div>
                    <button id="btn-apply-custom-years" onclick="applyCustomYearRange()"
                            style="padding: 6px 16px; background: #3b82f6; color: white; border: none; border-radius: 6px; font-weight: 500; cursor: pointer; font-size: 0.9em; transition: background 0.2s;">
                        Apply
                    </button>
                </div>
            </div>
        </div>

        <div id="time-series-chart-container" style="width: 100%%; height: 500px; position: relative;">
            <!-- Chart will be rendered by JavaScript -->
        </div>

        <div id="time-series-legend" style="margin-top: 24px; display: flex; flex-wrap: wrap; gap: 16px; justify-content: center;">
            <!-- Legend will be populated by JavaScript -->
        </div>

        <script type="application/json" id="time-series-data">
        %s
        </script>
        <script type="application/json" id="time-series-config">
        {
            "recentStartYear": %d,
            "recentEndYear": %d,
            "historicalStartYear": %d,
            "historicalEndYear": %d,
            "deliveryYear": %d
        }
        </script>
    </div>',
    recent_start_year, recent_end_year,
    historical_start_year, delivery_year,
    time_series_json,
    recent_start_year, recent_end_year,
    historical_start_year, historical_end_year,
    delivery_year
  )
}

#' Build vocabulary harmonization section
build_vocabulary_harmonization_section <- function(metrics, has_delivery_data = TRUE) {

  # Return unavailable message if delivery data is missing
  if (!has_delivery_data) {
    return(build_data_unavailable_message("Delivery", "Vocabulary Harmonization"))
  }

  # Overall source vocabularies
  overall_source_vocab <- metrics$source_vocabularies %>%
    dplyr::group_by(vocabulary) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(count)) %>%
    head(10)

  source_vocab_rows <- if (nrow(overall_source_vocab) > 0) {
    paste(sapply(1:nrow(overall_source_vocab), function(i) {
      sprintf('<tr><td>%s</td><td>%s</td></tr>',
              overall_source_vocab$vocabulary[i],
              format(overall_source_vocab$count[i], big.mark = ","))
    }), collapse = "\n")
  } else {
    '<tr><td colspan="2">No vocabulary data available</td></tr>'
  }

  # Overall target vocabularies
  overall_target_vocab <- metrics$target_vocabularies %>%
    dplyr::group_by(vocabulary) %>%
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(desc(count)) %>%
    head(10)

  target_vocab_rows <- if (nrow(overall_target_vocab) > 0) {
    paste(sapply(1:nrow(overall_target_vocab), function(i) {
      sprintf('<tr><td>%s</td><td>%s</td></tr>',
              overall_target_vocab$vocabulary[i],
              format(overall_target_vocab$count[i], big.mark = ","))
    }), collapse = "\n")
  } else {
    '<tr><td colspan="2">No vocabulary data available</td></tr>'
  }

  sprintf('
    <div class="section" id="vocab-harmonization">
        <div class="section-header">
            <span class="section-icon"></span>
            <h2>Vocabulary Harmonization</h2>
        </div>

        <div style="display: flex; gap: 32px; margin-bottom: 30px; justify-content: center; flex-wrap: wrap;">
            <div class="metric-card" style="flex: 0 1 400px;">
                <div class="metric-label">Delivered Vocabulary Version</div>
                <div class="metric-value" style="font-size: 1.1em;">%s</div>
            </div>

            <div class="metric-card" style="flex: 0 1 400px;">
                <div class="metric-label">Standardized to Vocabulary Version</div>
                <div class="metric-value" style="font-size: 1.1em;">%s</div>
            </div>
        </div>

        <div id="vocab-harm-content">
            <!-- Dynamically populated by JavaScript -->
        </div>

        <div class="subsection" style="margin-top: 40px;">
            <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 40px; margin-top: 24px;">
                <div>
                    <h4 style="margin-bottom: 16px;">Top Source Vocabularies</h4>
                    <div class="table-container" style="border: 2px solid #e2e8f0; border-radius: 8px;">
                        <table class="vocab-table">
                            <thead style="background: linear-gradient(135deg, #f1f5f9 0%%, #e2e8f0 100%%);">
                                <tr>
                                    <th>Vocabulary</th>
                                    <th>Count</th>
                                </tr>
                            </thead>
                            <tbody>
                                %s
                            </tbody>
                        </table>
                    </div>
                </div>
                <div>
                    <h4 style="margin-bottom: 16px;">Top Target Vocabularies</h4>
                    <div class="table-container" style="border: 2px solid #e2e8f0; border-radius: 8px;">
                        <table class="vocab-table">
                            <thead style="background: linear-gradient(135deg, #f1f5f9 0%%, #e2e8f0 100%%);">
                                <tr>
                                    <th>Vocabulary</th>
                                    <th>Count</th>
                                </tr>
                            </thead>
                            <tbody>
                                %s
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>',
    metrics$metadata$delivered_vocab_version,
    metrics$metadata$standardized_vocab_version,
    source_vocab_rows,
    target_vocab_rows
  )
}

#' Build table drilldown section (initially hidden)
build_table_drilldown_section <- function() {
  '
    <div class="section drill-down" id="table-drilldown" style="display: none;">
        <button class="back-button" onclick="hideTableDrilldown()">← Back to Delivery Report</button>

        <div class="section-header">
            <div>
                <h2 id="drilldown-table-name" style="margin: 0 0 4px 0; color: #0f172a;">Table Name</h2>
                <div style="font-size: 0.9em; color: #64748b; font-weight: 400;">Table Drilldown Report</div>
            </div>
        </div>

        <div id="drilldown-content">
            <!-- Dynamically populated by JavaScript -->
        </div>
    </div>'
}

#' Build DQD grid section
build_dqd_grid_section <- function(dqd_scores, has_dqd_data = TRUE) {

  # Return unavailable message if DQD data is missing
  if (!has_dqd_data) {
    return(build_data_unavailable_message("DQD", "Data Quality Results"))
  }

  # Reshape for display
  grid_wide <- dqd_scores$grid %>%
    tidyr::pivot_wider(names_from = context, values_from = c(Pass, Fail, Total, `% Pass`))

  categories <- c("Plausibility", "Conformance", "Completeness", "Total")

  grid_rows <- paste(sapply(categories, function(cat_name) {
    row_data <- grid_wide %>% dplyr::filter(category == cat_name)

    if (nrow(row_data) > 0) {
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
    } else {
      ""
    }
  }), collapse = "\n")

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

#' Build technical summary section
build_technical_summary_section <- function(metrics, has_delivery_data = TRUE) {

  # Return unavailable message if delivery data is missing
  if (!has_delivery_data) {
    return(build_data_unavailable_message("Delivery", "Technical Summary"))
  }
  sprintf('
    <div class="section" id="technical-summary">
        <div class="section-header">
            <span class="section-icon">⚙️</span>
            <h2>Technical Summary</h2>
        </div>

        <div style="display: flex; gap: 20px; margin-bottom: 30px; justify-content: center; flex-wrap: wrap;">
            <div class="metric-card" style="flex: 0 1 240px;">
                <div class="metric-label">Processing Date</div>
                <div class="metric-value" style="font-size: 1.5em;">%s</div>
            </div>

            <div class="metric-card" style="flex: 0 1 240px;">
                <div class="metric-label">Delivered CDM Version</div>
                <div class="metric-value" style="font-size: 1.5em;">%s</div>
            </div>

            <div class="metric-card" style="flex: 0 1 240px;">
                <div class="metric-label">Standardized CDM Version</div>
                <div class="metric-value" style="font-size: 1.5em;">%s</div>
            </div>

            <div class="metric-card" style="flex: 0 1 240px;">
                <div class="metric-label">File Format</div>
                <div class="metric-value" style="font-size: 1.5em;">%s</div>
            </div>
        </div>
        <p style="text-align: center; margin-top: 30px; color: #94a3b8; font-size: 0.9em;">Pipeline Version: %s</p>
    </div>',
    metrics$metadata$processing_date,
    metrics$metadata$delivered_cdm_version,
    metrics$metadata$standardized_cdm_version,
    toupper(metrics$metadata$file_format),
    metrics$metadata$pipeline_version
  )
}
