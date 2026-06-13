#' Create a stable filename for an observational snapshot
#'
#' Generates a filesystem-safe, chronologically sortable filename
#' for storing observational snapshot artefacts.
#'
#' The filename encodes:
#'
#' - storage context (`storage_id`)
#' - optional observational scope (`label`)
#' - scan timestamp
#' - a short deterministic uniqueness hash
#'
#' @details
#' The generated filename acts as a lightweight external identifier
#' for a stored observational snapshot.
#'
#' The naming scheme is intended to support:
#'
#' - reproducible archival storage
#' - chronological ordering
#' - provenance-aware reconstruction
#' - comparison of repeated filesystem observations
#'
#' The generated hash is deterministic for identical inputs and helps
#' avoid filename collisions between similar scan events.
#'
#' The function does not assign documentary identity or Record Set
#' semantics. It creates identifiers for stored observational artefacts.
#'
#' @param storage_id Character. Identifier of the storage context.
#' @param scan_time POSIXct. Timestamp of the observation event
#'   (default: current time).
#' @param label Character. Optional label describing the observed scope.
#'
#' @return Character. Snapshot filename ending in `.rds`.
#' @keywords internal
#' @importFrom digest digest
make_scan_filename <- function(storage_id,
                               scan_time = Sys.time(),
                               label = NULL) {
  # --- validate inputs ---
  if (!is.character(storage_id) || length(storage_id) != 1) {
    stop("make_scan_filename(): storage_id must be a single string", call. = FALSE)
  }

  if (!inherits(scan_time, "POSIXct")) {
    stop("make_scan_filename(): scan_time must be POSIXct", call. = FALSE)
  }

  if (!is.null(label) && !is.character(label)) {
    stop("make_scan_filename(): label must be character or NULL", call. = FALSE)
  }

  # --- normalize timestamp ---
  ts <- format(scan_time, "%Y%m%d-%H%M%S")

  # --- sanitize label ---
  label_part <- ""
  if (!is.null(label) && nzchar(label)) {
    label_clean <- tolower(label)
    label_clean <- gsub("[^a-z0-9]+", "_", label_clean)
    label_clean <- gsub("^_|_$", "", label_clean)
    label_part <- paste0("_", label_clean)
  }

  # --- deterministic observational hash ---
  entropy <- paste0(storage_id, ts, label_part)

  h <- substr(
    digest::digest(entropy, algo = "xxhash32"),
    1, 6
  )

  # --- assemble snapshot artefact filename ---
  paste0(
    "scan_",
    storage_id,
    label_part,
    "_",
    ts,
    "_",
    h,
    ".rds"
  )
}
