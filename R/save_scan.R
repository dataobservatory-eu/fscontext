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
#' tmp_dir <- tempfile()
#' dir.create(tmp_dir)
#'
#' dir.create(file.path(tmp_dir, "R"))
#' dir.create(file.path(tmp_dir, "data"))
#'
#' file.create(file.path(tmp_dir, "R", "a.R"))
#' file.create(file.path(tmp_dir, "R", "b.R"))
#' file.create(file.path(tmp_dir, "data", "c.csv"))
#'
#' scan <- scan_storage(
#'   root = tmp_dir,
#'   storage_id = "test-storage"
#' )
#'
#' save_scan(
#'   df = scan,
#'   storage_id = "test-storage",
#'   path = tmp_dir
#' )
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

  # Preserve the existing filename-generation algorithm.
  fname <- make_scan_filename(
    storage_id,
    scan_time,
    label = label
  )

  full_path <- file.path(path, fname)

  # Issue #5: never silently overwrite an existing snapshot.
  if (file.exists(full_path)) {
    stop(
      "save_scan(): snapshot already exists: ",
      full_path,
      call. = FALSE
    )
  }

  # Issue #6: stage serialization in the destination directory.
  tmp_path <- tempfile(
    pattern = ".fscontext-",
    tmpdir = path,
    fileext = ".rds"
  )

  # Remove the temporary file on both success and failure.
  on.exit(unlink(tmp_path), add = TRUE)

  saveRDS(df, tmp_path)

  # Verify that serialization produced a readable RDS.
  readRDS(tmp_path)

  # Publish without overwriting an existing destination.
  # file.link() fails if the destination already exists.
  # Both paths reside on the same filesystem.
  published <- file.link(tmp_path, full_path)

  if (!isTRUE(published)) {
    stop(
      "save_scan(): could not publish snapshot without overwriting: ",
      full_path,
      call. = FALSE
    )
  }

  message("Saved: ", full_path)

  invisible(full_path)
}
