#' Create and persist an observational snapshot of a filesystem
#'
#' Executes [scan_storage()] and stores the resulting observational dataset
#' as a timestamped `.rds` snapshot.
#'
#' @details
#' Each snapshot captures the **state of file instantiations at a specific
#' point in time**, preserving:
#'
#' - file-level metadata
#' - structural context
#' - globally contextualised file paths (`storage_id::local_path`)
#' - optional content signatures
#' - repository associations
#'
#' Snapshots are intended as:
#'
#' - durable audit artefacts
#' - inputs for longitudinal analysis
#' - reproducible evidence of observed environments
#'
#' Since schema version 0.1.3, snapshots store `full_path`
#' values as globally contextualised paths:
#'
#' `storage_id::local_filesystem_path`
#'
#' This prevents collisions between similar local folder structures
#' observed on different machines or storage contexts.
#'
#' @return Invisibly returns the path to the stored snapshot.
#' @param root Character. Path to the root folder to scan.
#' @param storage_id Character. Identifier of the storage.
#' @param person_id Character. Identifier of the person.
#' @param scan_time POSIXct. Timestamp of the scan.
#' @param label Character. Optional human-readable label describing
#'   the scanned scope (e.g. "d_eviota").
#' @param path Character. Directory where snapshots are stored.
#' @param compute_signature Logical. Whether to compute fast file signatures.
#' @param max_signature_size Numeric. Maximum file size in bytes for signatures.
#'
#' @return Invisibly returns the full path to the saved snapshot.
#'
#' @examples
#' \dontrun{
#' snapshot_storage("D:/_markdown", "l480-ssd", label = "d_markdown")
#' }
#'
#' @export
snapshot_storage <- function(
  root,
  storage_id = "l480-ssd",
  person_id = "antaldaniel",
  scan_time = Sys.time(),
  label = NULL,
  path = here::here("data-raw", "snapshots"),
  compute_signature = TRUE,
  max_signature_size = 200 * 1024 * 1024
) {
  df <- scan_storage(
    root = root,
    storage_id = storage_id,
    person_id = person_id,
    scan_time = scan_time,
    compute_signature = compute_signature,
    max_signature_size = max_signature_size
  )

  if (is.null(label) || !nzchar(label)) {
    root_norm <- fs::path_abs(root)

    drive <- tolower(substr(root_norm, 1, 1))

    folder <- basename(root_norm)

    label <- paste0(
      drive,
      "_",
      gsub("[^a-zA-Z0-9]+", "_", folder)
    )
  }

  out <- save_scan(
    df = df,
    storage_id = storage_id,
    path = path,
    label = label
  )

  invisible(out)
}
