# ==============================================================================
# CSS Styles for OMOP Delivery Report
# ==============================================================================

#' Get complete CSS styles for the report
#'
#' Reads CSS from external file rather than embedding as a string.
#' This allows proper syntax highlighting and easier maintenance.
#'
#' @return Character string containing the full CSS code
get_full_css_styles <- function() {
  # Use system.file() to find the CSS file in the installed package
  css_path <- system.file("css", "report.css", package = "omopDeliveryReport")

  if (css_path == "") {
    stop("Could not find css/report.css in installed package")
  }

  # Read the CSS file
  css_content <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  return(css_content)
}
