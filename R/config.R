# ==============================================================================
# Configuration Management for OMOP Delivery Reports
# ==============================================================================

#' Get default report configuration
#'
#' Returns default configuration for OMOP delivery report generation.
#' Override values by passing a partial configuration list to merge_config().
#'
#' @return List containing configuration parameters
#' @export
#'
#' @section Alert Thresholds:
#' \describe{
#'   \item{default_dates_pct}{Percentage threshold for default date warnings (default: 1)}
#'   \item{invalid_concepts_count}{Row count threshold for invalid concepts (default: 0)}
#' }
#'
#' @section Color Scheme:
#' Colorblind-friendly palette based on viridis and RColorBrewer.
#'
#' @examples
#' config <- default_config()
#' config$thresholds$default_dates_pct <- 5  # More lenient threshold
default_config <- function() {
  list(
    # Alert thresholds
    thresholds = list(
      default_dates_pct = 1,          # Show alert if >1% rows have default dates
      invalid_concepts_count = 0,     # Show alert if any rows have invalid concepts
      row_count_mismatch = TRUE       # Always show row count mismatch alerts
    ),

    # Color scheme (colorblind-friendly)
    colors = list(
      type_concepts = list(
        ehr = "#0073C2",
        claims = "#EFC000",
        disease_registry = "#868686",
        patient_reported = "#CD534C",
        unlabeled = "#2d2d2d",
        other = "#7AA6DC"
      ),
      dqd_scores = list(
        good_threshold = 95,    # >= 95% is "good"
        fair_threshold = 85,    # >= 85% is "fair", below is "poor"
        good_color = "#10b981",
        fair_color = "#f59e0b",
        poor_color = "#ef4444"
      )
    ),

    # Table groupings for CDM
    table_groups = list(
      "Clinical Data" = c(
        "person", "visit_occurrence", "visit_detail", "condition_occurrence",
        "drug_exposure", "procedure_occurrence", "device_exposure",
        "measurement", "observation", "death", "note", "specimen"
      ),
      "Health System" = c("location", "care_site", "provider"),
      "Healthcare Economics" = c("payer_plan_period", "cost"),
      "Derived Data" = c("observation_period", "drug_era", "condition_era", "dose_era"),
      "Vocabulary" = c(
        "concept", "vocabulary", "domain", "concept_class",
        "concept_relationship", "relationship", "concept_synonym",
        "concept_ancestor", "source_to_concept_map", "drug_strength"
      ),
      "Metadata" = c("metadata", "cdm_source"),
      "Other" = c(
        "note_nlp", "fact_relationship", "cohort", "cohort_definition",
        "episode", "episode_event", "attribute_definition"
      )
    ),

    # Tables derived by pipeline (skip row count validation warnings)
    pipeline_derived_tables = c(
      "condition_era", "drug_era", "dose_era", "observation_period", "cdm_source"
    ),

    # Type concept grouping rules
    type_concept_groups = list(
      ehr = list(
        patterns = "EHR",
        case_sensitive = TRUE
      ),
      claims = list(
        patterns = c("claim", "payer system record"),
        case_sensitive = FALSE
      ),
      disease_registry = list(
        exact_matches = c("Registry", "Tumor Registry"),
        case_sensitive = TRUE
      ),
      patient_reported = list(
        exact_matches = c(
          "Patient self-report", "Patient self-tested",
          "Patient filled survey", "Survey",
          "Patient Self-Reported Medication"
        ),
        case_sensitive = TRUE
      ),
      unlabeled = list(
        exact_matches = c("No matching concept", "0", ""),
        case_sensitive = FALSE
      )
    )
  )
}

#' Merge user configuration with defaults
#'
#' Recursively merges user-specified configuration with default values.
#' User values override defaults.
#'
#' @param user_config List of user-specified configuration options
#' @return Merged configuration list
#' @export
#'
#' @examples
#' user_cfg <- list(thresholds = list(default_dates_pct = 5))
#' cfg <- merge_config(user_cfg)
merge_config <- function(user_config = list()) {
  default <- default_config()
  modifyList(default, user_config)
}

#' Get alert threshold for default dates
#'
#' @param config Configuration list from merge_config()
#' @return Numeric threshold percentage
get_default_dates_threshold <- function(config = default_config()) {
  config$thresholds$default_dates_pct
}

#' Get alert threshold for invalid concepts
#'
#' @param config Configuration list from merge_config()
#' @return Numeric threshold count
get_invalid_concepts_threshold <- function(config = default_config()) {
  config$thresholds$invalid_concepts_count
}

#' Get type concept color mapping
#'
#' @param config Configuration list from merge_config()
#' @return Named character vector of hex colors
get_type_concept_colors <- function(config = default_config()) {
  unlist(config$colors$type_concepts)
}

#' Get DQD score classification thresholds
#'
#' @param config Configuration list from merge_config()
#' @return List with good_threshold and fair_threshold
get_dqd_thresholds <- function(config = default_config()) {
  list(
    good = config$colors$dqd_scores$good_threshold,
    fair = config$colors$dqd_scores$fair_threshold
  )
}

#' Classify DQD score
#'
#' @param score Numeric DQD score (0-100)
#' @param config Configuration list from merge_config()
#' @return Character: "good", "fair", or "poor"
classify_dqd_score <- function(score, config = default_config()) {
  thresholds <- get_dqd_thresholds(config)
  if (score >= thresholds$good) {
    "good"
  } else if (score >= thresholds$fair) {
    "fair"
  } else {
    "poor"
  }
}
