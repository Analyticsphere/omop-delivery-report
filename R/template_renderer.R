# ==============================================================================
# Template Rendering System
# ==============================================================================
# Simple, clean HTML template rendering with variable substitution.
# Templates use {{variable}} syntax for placeholders.

#' Load a template file from inst/templates
#'
#' @param template_name Template path relative to inst/templates (e.g., "sections/header")
#' @return Character string containing template content
load_template <- function(template_name) {
  # Add .html extension if not present
  if (!grepl("\\.html$", template_name)) {
    template_name <- paste0(template_name, ".html")
  }

  # Construct full path
  template_path <- system.file("templates", template_name, package = "omopDeliveryReport")

  # Check if template exists
  if (template_path == "" || !file.exists(template_path)) {
    stop("Template not found: ", template_name)
  }

  # Read template content
  template_content <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  return(template_content)
}

#' Substitute variables in a template string
#'
#' Replaces {{variable}} placeholders with values from data list.
#' Supports nested data access (e.g., {{metadata.site}}).
#'
#' @param template Character string containing template with {{variable}} placeholders
#' @param data Named list of variables to substitute
#' @return Character string with variables substituted
substitute_variables <- function(template, data) {
  if (length(data) == 0) return(template)

  result <- template

  # Process each variable in the data list
  for (key in names(data)) {
    value <- data[[key]]

    # Convert value to string
    if (is.null(value)) {
      value_str <- ""
    } else if (is.numeric(value)) {
      value_str <- as.character(value)
    } else if (is.character(value)) {
      value_str <- value
    } else {
      value_str <- as.character(value)
    }

    # Replace all occurrences of {{key}}
    pattern <- paste0("{{", key, "}}")
    result <- gsub(pattern, value_str, result, fixed = TRUE)
  }

  return(result)
}

#' Render a template with data
#'
#' Main entry point for template rendering. Loads template and substitutes variables.
#'
#' @param template_name Template path relative to inst/templates
#' @param data Named list of variables to substitute (default: empty list)
#' @return Character string containing rendered HTML
#' @examples
#' \dontrun{
#'   render_template("sections/header", list(
#'     site_name = "My Hospital",
#'     delivery_date = "2025-01-01"
#'   ))
#' }
render_template <- function(template_name, data = list()) {
  # Load template
  template <- load_template(template_name)

  # Substitute variables
  html <- substitute_variables(template, data)

  return(html)
}

#' Render multiple items using a component template
#'
#' Utility for rendering lists of items (e.g., table rows, metric cards).
#' Renders the component template once for each item.
#'
#' @param component_name Component template name
#' @param items List of data lists, one per item
#' @return Character string with all rendered items concatenated
#' @examples
#' \dontrun{
#'   render_component_list("components/table_row", list(
#'     list(name = "person", count = 1000),
#'     list(name = "visit", count = 5000)
#'   ))
#' }
render_component_list <- function(component_name, items) {
  if (length(items) == 0) return("")

  # Load component template once
  template <- load_template(component_name)

  # Render each item
  rendered_items <- sapply(items, function(item) {
    substitute_variables(template, item)
  })

  # Concatenate with newlines
  paste(rendered_items, collapse = "\n")
}

# ==============================================================================
# Asset Loaders
# ==============================================================================

#' Get complete JavaScript for the report
#'
#' Reads JavaScript from external file for syntax highlighting and external editing.
#'
#' @return Character string containing the full JavaScript code
#' @export
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

#' Get complete CSS styles for the report
#'
#' Reads CSS from external file for syntax highlighting and external editing.
#'
#' @return Character string containing the full CSS code
#' @export
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
