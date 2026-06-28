#' Persist an observational snapshot to disk
#'
#' Saves a snapshot dataset (typically produced by [scan_storage()])
#' as a uniquely identified `.rds` file.
#'
#' @details
#' The filename encodes:
#'
#' - storage context
#' - observation time
#' - optional scope label
#' - a deterministic hash
#'
#' This ensures:
#'
#' - chronological ordering
#' - uniqueness
#' - reproducibility of stored observations
#'
#' @param df Data.frame. Scan result with created with [scan_storage()] that
#' has a `created_at` attribute.
#' @param storage_id Character. Identifier of the storage.
#' @param path Character. Directory where the file will be saved.
#' @param label Character. Optional human-readable label describing
#'   the scanned scope (e.g. "d_eviota"). Default is \code{NULL} (no label).
#'
#' @return Invisibly returns the full file path of the saved `.rds` file.
#'
#' @importFrom fs dir_exists dir_create
#' @examples
#' root <- tempfile()
#' dir.create(root)
#'
#' dir.create(file.path(root, "R"))
#' dir.create(file.path(root, "data"))
#'
#' file.create(file.path(root, "R", "a.R"))
#' file.create(file.path(root, "R", "b.R"))
#' file.create(file.path(root, "data", "c.csv"))
#'
#' scan_storage(
#'   root = root,
#'   storage_id = "test-storage",
#'   path = tmp
#' )
#'
#' save_scan(scan, "test-storage")
#'
#' @export
save_scan <- function(df,
                      storage_id,
                      path,
                      label = NULL) {
  if (missing(path) || is.null(path) || !is.character(path)) {
    stop("save_scan(): 'path' must be supplied explicitly.", call. = FALSE)
  }

  if (is.null(attr(df, "created_at"))) {
    stop("save_scan(): df must have a 'created_at' attribute", call. = FALSE)
  }

  if (!is.null(label) && !is.character(label)) {
    stop("save_scan(): label must be character or NULL", call. = FALSE)
  }

  if (!fs::dir_exists(path)) {
    fs::dir_create(path, recurse = TRUE)
  }

  scan_time <- attr(df, "created_at")

  fname <- make_scan_filename(storage_id, scan_time, label = label)

  full_path <- file.path(path, fname)

  saveRDS(df, full_path)

  message("Saved: ", full_path)

  invisible(full_path)
}
