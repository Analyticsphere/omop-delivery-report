# ==============================================================================
# Run OMOP Delivery Report Generator
# ==============================================================================
#
# This script generates an OMOP delivery report using paths from environment
# variables. Paths can be local filesystem or Google Cloud Storage (gs://).
#
# Usage:
#   # Local paths
#   DELIVERY_REPORT_PATH=delivery_report.csv \
#   DQD_RESULTS_PATH=dqd_results.csv \
#   OUTPUT_PATH=omop_delivery_report.html \
#   Rscript run_full_report.R
#
#   # GCS paths
#   DELIVERY_REPORT_PATH=gs://bucket/data/delivery.csv \
#   DQD_RESULTS_PATH=gs://bucket/data/dqd.csv \
#   OUTPUT_PATH=gs://bucket/reports/output.html \
#   Rscript run_full_report.R
#
# Environment Variables:
#   DELIVERY_REPORT_PATH - Path to delivery report CSV (required)
#   DQD_RESULTS_PATH     - Path to DQD results CSV (optional)
#   OUTPUT_PATH          - Path for output HTML file (required)
# ==============================================================================

library(logger)

# Configure logger
log_appender(appender_stdout)
log_threshold(INFO)

# Source the complete report generator
source("R/generate_full_report.R")

# Read paths from environment variables with defaults
delivery_report_path <- Sys.getenv("DELIVERY_REPORT_PATH", "delivery_report.csv")
dqd_results_path <- Sys.getenv("DQD_RESULTS_PATH", "dqd_results.csv")
output_path <- Sys.getenv("OUTPUT_PATH", "omop_delivery_report_full.html")

# Log execution details
log_info("Starting OMOP Delivery Report generation")
log_info("Delivery report path: {delivery_report_path}")
log_info("DQD results path: {dqd_results_path}")
log_info("Output path: {output_path}")

# Generate the report
tryCatch({
  generate_full_omop_report(
    delivery_report_path = delivery_report_path,
    dqd_results_path = dqd_results_path,
    output_path = output_path
  )

  log_info("Report generation completed successfully")
}, error = function(e) {
  log_error("Report generation failed: {e$message}")
  quit(status = 1)
})