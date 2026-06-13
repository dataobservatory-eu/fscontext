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
#' @param df Data.frame. Scan result with a `created_at` attribute.
#' @param storage_id Character. Identifier of the storage.
#' @param path Character. Directory where the file will be saved.
#' @param label Character. Optional human-readable label describing
#'   the scanned scope (e.g. "d_eviota").
#'
#' @return Invisibly returns the full file path of the saved `.rds` file.
#'
#' @importFrom fs dir_exists dir_create
#' @importFrom here here
#' @examples
#' \dontrun{
#' scan <- scan_storage("D:/_markdown")
#' save_scan(scan, "l480-ssd")
#' }
#'
#' @export
save_scan <- function(df,
                      storage_id,
                      path = here::here("data-raw", "snapshots"),
                      label = NULL) {
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
