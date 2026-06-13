#' Normalise snapshot schema to the current version
#'
#' Ensures that a snapshot `data.frame` conforms to the canonical
#' schema used by the package.
#'
#' This function detects the schema version of the input (via the
#' `schema_version` attribute) and applies the necessary migration
#' steps to bring it to the current version.
#'
#' Currently supported:
#'
#' - snapshots without a `schema_version` attribute are assumed to
#'   originate from version 0.1.0 and are migrated accordingly
#' - snapshots with `schema_version = "0.1.0"` are migrated to
#'   version 0.1.2
#'
#' The function is safe to call on already-normalised data and may
#' be used at the start of analytical workflows to ensure consistency.
#'
#' @param df A `data.frame` representing a filesystem snapshot.
#'
#' @return A `data.frame` conforming to the current snapshot schema.
#'
#' @details
#' The canonical schema includes:
#'
#' - `rel_path` as the primary file-instance identifier
#' - `filename` as the basename of the file
#' - `dir_path` derived from `rel_path`
#'
#' Additional columns from `scan_storage()` are preserved.
#'
#' @keywords internal
#' @noRd

normalise_snapshot_schema <- function(df) {
  version <- attr(df, "schema_version")

  # assume legacy if unknown because of 0.1.0 version snapshots
  if (is.null(version) || version == "0.1.0") {
    df <- normalise_snapshot_schema_010(df)
  }

  df
}

#' Normalise snapshot schema from version 0.1.0 to 0.1.2
#'
#' Migrates a snapshot `data.frame` created with earlier versions of the
#' package (pre-0.1.2) to the canonical schema used from version 0.1.2 onward.
#'
#' This function performs non-destructive transformations to align column names
#' and derived variables with the current naming contract:
#'
#' - derives `dir_path` from `rel_path` if missing
#' - renames `file` to `filename` where applicable
#' - renames `folder` to `group_path` where applicable
#'
#' The function is designed to be:
#'
#' - idempotent (safe to run multiple times)
#' - conservative (does not overwrite existing canonical columns)
#' - explicit (warns on conflicting legacy vs canonical values)
#'
#' After normalisation, the function attaches schema metadata as attributes:
#'
#' - `schema_version = "0.1.2"`
#' - `normalised_from = "0.1.0"`
#'
#' @param df A `data.frame` representing a filesystem snapshot.
#'
#' @return A `data.frame` with canonical column names and derived variables.
#'
#' @details
#' This function does not remove legacy columns (`file`, `folder`) but ensures
#' that canonical columns (`filename`, `group_path`) are present and consistent.
#'
#' The resulting object conforms to the schema described in the package
#' vignette ("Reporting Data Structure").
#'
#' @keywords internal
#' @noRd
normalise_snapshot_schema_010 <- function(df) {
  # --- copy to avoid accidental reference issues ---
  out <- df

  # --- derive dir_path safely ---
  if (!"dir_path" %in% names(out) && "rel_path" %in% names(out)) {
    out$dir_path <- fs::path_dir(out$rel_path)
    out$dir_path[out$dir_path == "."] <- ""
  }

  # --- derive filename from rel_path if missing ---
  if (!"filename" %in% names(out) && "rel_path" %in% names(out)) {
    out$filename <- basename(out$rel_path)
  }

  # --- rename file -> filename (safe) ---
  if ("file" %in% names(out)) {
    if (!"filename" %in% names(out)) {
      out$filename <- out$file
    } else {
      if (!all(out$filename == out$file, na.rm = TRUE)) {
        warning("Conflict between 'file' and 'filename'; keeping 'filename'")
      }
    }
  }

  # --- rename folder -> group_path (safe) ---
  if ("folder" %in% names(out)) {
    if (!"group_path" %in% names(out)) {
      out$group_path <- out$folder
    } else {
      if (!all(out$group_path == out$folder, na.rm = TRUE)) {
        warning("Conflict between 'folder' and 'group_path'; keeping 'group_path'")
      }
    }
  }

  # --- ensure core columns exist ---
  required <- c("rel_path", "filename")
  missing <- setdiff(required, names(out))

  if (length(missing) > 0) {
    stop(
      "Missing required columns after normalisation: ",
      paste(missing, collapse = ", ")
    )
  }

  # --- attach migration metadata ---
  attr(out, "schema_version") <- "0.1.2"
  attr(out, "normalised_from") <- "0.1.0"

  out
}
