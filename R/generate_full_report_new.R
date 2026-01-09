# ==============================================================================
# Main Report Generation Entry Point
# ==============================================================================
# Clean, orchestrated workflow for generating OMOP delivery reports.
# This file demonstrates professional architecture and serves as an example for junior developers.

#' Generate complete OMOP delivery report
#'
#' Main entry point for report generation. Loads data, calculates metrics,
#' and generates an HTML report with embedded JavaScript and CSS.
#'
#' Workflow:
#'   1. Load and validate input data (CSV files)
#'   2. Calculate all metrics and scores
#'   3. Transform data for display
#'   4. Build HTML report from templates
#'   5. Write output file
#'
#' @param delivery_report_path Path to delivery_report.csv (local or gs:// URI)
#' @param dqd_results_path Path to dqd_results.csv (local or gs:// URI)
#' @param output_path Path for output HTML file (local or gs:// URI)
#' @param config Configuration list (default: default_config())
#' @return Invisible NULL (writes file as side effect)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Local files
#'   generate_full_omop_report(
#'     "data/delivery_report.csv",
#'     "data/dqd_results.csv",
#'     "reports/omop_report.html"
#'   )
#'
#'   # GCS paths
#'   generate_full_omop_report(
#'     "gs://my-bucket/delivery_report.csv",
#'     "gs://my-bucket/dqd_results.csv",
#'     "gs://my-bucket/report.html"
#'   )
#' }
generate_full_omop_report_refactored <- function(
  delivery_report_path = "delivery_report.csv",
  dqd_results_path = "dqd_results.csv",
  output_path = "omop_delivery_report.html",
  config = default_config()
) {

  # Print header
  cat("\n")
  cat("============================================================\n")
  cat("OMOP DELIVERY REPORT GENERATOR\n")
  cat("============================================================\n\n")

  # ============================================================================
  # STEP 1: LOAD DATA
  # ============================================================================
  cat("Loading data files...\n")

  delivery_data <- load_delivery_data(delivery_report_path)
  dqd_data <- load_dqd_data(dqd_results_path)

  # Check data availability
  availability <- check_data_availability(delivery_data, dqd_data)
  has_delivery_data <- availability$has_delivery_data
  has_dqd_data <- availability$has_dqd_data

  cat("Delivery data available: ", has_delivery_data, "\n", sep = "")
  cat("DQD data available: ", has_dqd_data, "\n", sep = "")

  # Handle case where neither data source is available
  if (!has_delivery_data && !has_dqd_data) {
    stop("Both delivery report and DQD results are missing. Cannot generate report.")
  }

  # ============================================================================
  # STEP 2: PARSE AND CALCULATE METRICS
  # ============================================================================
  cat("Parsing delivery metrics...\n")

  # Parse delivery data (if available)
  metrics <- if (has_delivery_data) {
    parsed <- parse_delivery_metrics(delivery_data)
    # Group type concepts
    parsed$type_concepts_grouped <- group_type_concepts(parsed$type_concepts)
    parsed
  } else {
    # Create empty metrics structure
    create_empty_metrics()
  }

  # ============================================================================
  # STEP 3: CALCULATE DQD SCORES
  # ============================================================================
  cat("Calculating DQD scores...\n")

  # Get table groups
  table_groups <- get_table_groups(config)
  table_groups_with_all <- add_all_tables_group(table_groups)

  # Calculate DQD scores
  if (has_dqd_data) {
    dqd_scores <- list(
      overall = calculate_overall_dqd_score(dqd_data),
      grid = create_dqd_grid(dqd_data)
    )
    group_dqd_scores <- calculate_all_group_dqd_scores(dqd_data, table_groups_with_all)
    table_dqd_scores <- if (has_delivery_data) {
      calculate_all_table_dqd_scores(dqd_data, metrics$valid_tables$table_name)
    } else {
      list()
    }
  } else {
    dqd_scores <- create_empty_dqd_scores()
    group_dqd_scores <- lapply(table_groups_with_all, function(x) NA_real_)
    table_dqd_scores <- list()
  }

  # ============================================================================
  # STEP 4: BUILD HTML REPORT
  # ============================================================================
  cat("Building HTML structure...\n")

  html <- build_complete_html_report(
    metrics = metrics,
    dqd_data = dqd_data,
    dqd_scores = dqd_scores,
    table_groups = table_groups_with_all,
    group_dqd_scores = group_dqd_scores,
    table_dqd_scores = table_dqd_scores,
    has_delivery_data = has_delivery_data,
    has_dqd_data = has_dqd_data
  )

  # ============================================================================
  # STEP 5: WRITE OUTPUT
  # ============================================================================
  cat("Writing HTML file...\n")

  write_file(html, output_path)

  # ============================================================================
  # SUMMARY
  # ============================================================================
  cat("\nReport generation complete!\n\n")

  if (has_delivery_data) {
    cat("Report Details:\n")
    cat("Site: ", metrics$metadata$site, "\n", sep = "")
    cat("Delivery Date: ", metrics$metadata$delivery_date, "\n", sep = "")
    if (has_dqd_data) {
      cat("Overall DQD Score: ", dqd_scores$overall, "%\n", sep = "")
    }
    cat("Tables Delivered: ", nrow(metrics$valid_tables), "\n", sep = "")
  }

  invisible(NULL)
}
