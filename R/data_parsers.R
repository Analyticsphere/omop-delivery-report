# ==============================================================================
# Data Parsers
# ==============================================================================
# Parse raw CSV data into structured R data frames.
# These functions extract and structure metrics from the delivery report CSV.

#' Parse delivery report metrics from CSV data
#'
#' Extracts all metrics from the delivery report CSV into a structured list.
#' This is the main entry point for transforming raw CSV data into usable metrics.
#'
#' @param delivery_data Data frame with name, value_as_string, value_as_number columns
#' @return List containing all parsed metrics
#' @export
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
#' This function is typically called after parse_delivery_metrics() to add type_group
#' classifications to the type_concepts data.
#'
#' @param type_concepts Data frame with type_concept column
#' @return Data frame with added type_group column
#' @export
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

#' Create empty metrics structure
#'
#' Used when delivery data is not available. Creates a metrics list with all
#' expected fields populated with empty data structures.
#'
#' @return List with empty metric structures matching parse_delivery_metrics() output
#' @export
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
