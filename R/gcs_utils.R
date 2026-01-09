# ==============================================================================
# Google Cloud Storage Utilities
# ==============================================================================
# Smart I/O functions that work with both local filesystem and GCS paths.
# Paths starting with "gs://" trigger GCS operations, others use local filesystem.

library(googleCloudStorageR)
library(logger)

#' Check if path is a GCS path
#'
#' @param path Character path to check
#' @return Logical TRUE if path starts with "gs://"
is_gcs_path <- function(path) {
  if (is.null(path) || length(path) == 0 || !is.character(path)) {
    return(FALSE)
  }
  grepl("^gs://", path)
}

#' Parse GCS path into bucket and object name
#'
#' Handles formats:
#'   - gs://bucket/path/to/file.csv → list(bucket="bucket", object="path/to/file.csv")
#'   - gs://bucket/file.csv → list(bucket="bucket", object="file.csv")
#'
#' @param gcs_path Character GCS URI (must start with gs://)
#' @return List with bucket and object components
parse_gcs_path <- function(gcs_path) {
  if (!is_gcs_path(gcs_path)) {
    stop("Path must start with gs://")
  }

  # Remove gs:// prefix
  clean_path <- sub("^gs://", "", gcs_path)

  # Split into bucket and object
  parts <- strsplit(clean_path, "/", fixed = TRUE)[[1]]

  if (length(parts) < 2) {
    stop("GCS path must include bucket and object: gs://bucket/object")
  }

  list(
    bucket = parts[1],
    object = paste(parts[-1], collapse = "/")
  )
}

#' Authenticate to GCS using service account credentials
#'
#' Authenticates using Cloud Run service account at /secrets/service-account.json
#' or local default credentials.
#'
#' @return Logical TRUE if authentication succeeded, FALSE otherwise
gcs_authenticate <- function() {
  tryCatch({
    # Check for Cloud Run service account JSON file
    service_account_path <- "/secrets/service-account.json"

    if (file.exists(service_account_path)) {
      # Cloud Run environment - use explicit JSON file
      googleCloudStorageR::gcs_auth(json_file = service_account_path)
      logger::log_debug("Authenticated to GCS using service account JSON: {service_account_path}")
    } else {
      # Local development - use default credential discovery
      googleCloudStorageR::gcs_auth()
      logger::log_debug("Authenticated to GCS using default credentials")
    }

    return(TRUE)
  }, error = function(e) {
    logger::log_warn("GCS authentication failed: {e$message}")
    return(FALSE)
  })
}

#' Read CSV from local filesystem or GCS
#'
#' Automatically detects path type and reads accordingly.
#' GCS files are downloaded to temp location, read, then deleted.
#'
#' @param path Character path (local or gs:// URI)
#' @return Data frame from CSV, or NULL if file doesn't exist
read_csv <- function(path) {
  if (is_gcs_path(path)) {
    # GCS mode
    gcs_authenticate()

    parsed <- parse_gcs_path(path)
    logger::log_info("Reading CSV from GCS: {path}")

    # Check if object exists
    obj_exists <- tryCatch({
      googleCloudStorageR::gcs_list_objects(
        bucket = parsed$bucket,
        prefix = parsed$object,
        detail = "summary"
      )
      TRUE
    }, error = function(e) {
      logger::log_warn("GCS object not found: {path}")
      FALSE
    })

    if (!obj_exists) {
      return(NULL)
    }

    # Download to temp file
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    tryCatch({
      googleCloudStorageR::gcs_get_object(
        object_name = parsed$object,
        bucket = parsed$bucket,
        saveToDisk = temp_file
      )

      data <- read.csv(temp_file, stringsAsFactors = FALSE)
      logger::log_info("Successfully read {nrow(data)} rows from GCS")
      return(data)

    }, error = function(e) {
      logger::log_error("Failed to read from GCS: {e$message}")
      return(NULL)
    })

  } else {
    # Local filesystem mode
    if (!file.exists(path)) {
      logger::log_warn("Local file not found: {path}")
      return(NULL)
    }

    tryCatch({
      data <- read.csv(path, stringsAsFactors = FALSE)
      logger::log_info("Read {nrow(data)} rows from local file: {path}")
      return(data)
    }, error = function(e) {
      logger::log_error("Failed to read local file: {e$message}")
      return(NULL)
    })
  }
}

#' Write file to local filesystem or GCS
#'
#' Automatically detects path type and writes accordingly.
#' For GCS, writes to temp file first, then uploads with retry logic.
#'
#' @param content Character content to write
#' @param path Character destination path (local or gs:// URI)
#' @param max_retries Integer maximum upload retry attempts for GCS (default: 3)
#' @return Logical TRUE on success
write_file <- function(content, path, max_retries = 3) {
  if (is_gcs_path(path)) {
    # GCS mode
    gcs_authenticate()

    parsed <- parse_gcs_path(path)
    logger::log_info("Writing file to GCS: {path}")

    # Write to temp file first
    temp_file <- tempfile(fileext = ".html")
    on.exit(unlink(temp_file), add = TRUE)

    writeLines(content, temp_file)
    file_size_kb <- round(file.info(temp_file)$size / 1024, 1)
    logger::log_info("File size: {file_size_kb} KB")

    # Upload with retry logic
    last_error <- NULL
    for (attempt in 1:max_retries) {
      tryCatch({
        if (attempt > 1) {
          logger::log_info("Upload attempt {attempt}/{max_retries}")
        }

        googleCloudStorageR::gcs_upload(
          file = temp_file,
          bucket = parsed$bucket,
          name = parsed$object,
          predefinedAcl = "bucketLevel"  # Use bucket-level IAM
        )

        logger::log_info("Successfully uploaded to GCS: {path}")
        return(TRUE)

      }, error = function(e) {
        last_error <<- e
        logger::log_warn("Upload attempt {attempt} failed: {e$message}")

        if (attempt < max_retries) {
          wait_time <- 2^attempt
          logger::log_info("Waiting {wait_time} seconds before retry...")
          Sys.sleep(wait_time)
        }
      })
    }

    # All retries failed
    logger::log_error("Failed to upload after {max_retries} attempts: {last_error$message}")
    stop("GCS upload failed: ", last_error$message)

  } else {
    # Local filesystem mode
    tryCatch({
      writeLines(content, path)
      file_size_kb <- round(file.info(path)$size / 1024, 1)
      logger::log_info("Wrote {file_size_kb} KB to local file: {path}")
      return(TRUE)
    }, error = function(e) {
      logger::log_error("Failed to write local file: {e$message}")
      stop("Failed to write file: ", e$message)
    })
  }
}

