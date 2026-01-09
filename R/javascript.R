# ==============================================================================
# JavaScript for OMOP Delivery Report Interactivity
# ==============================================================================

#' Get complete JavaScript for the report
#'
#' Reads JavaScript from external file for syntax highlighting and external editing.
#'
#' @return Character string containing the full JavaScript code
get_full_javascript <- function() {
  # Use system.file() to find the JavaScript file in the installed package
  js_path <- system.file("js", "report.js", package = "omopDeliveryReport")

  if (js_path == "") {
    stop("Could not find js/report.js in installed package")
  }

  # Read the JavaScript file
  js_content <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  return(js_content)
}
