#' Summarise file activity by time period and structural path
#'
#' Aggregates file-level observations (e.g. from `scan_storage()`) into
#' time-based summaries grouped by a deterministic structural path prefix.
#'
#' The function derives:
#'
#' - a *time bucket* (`period`) from file modification times (`mtime`)
#' - a *grouping key* (`group_path`) derived from the project and its
#'   immediate subdirectory (module), using an internal structural parser
#'
#' and summarises activity within each (period × group_path) combination.
#'
#' This provides a reproducible, structure-aware view of observed activity,
#' suitable for exploratory analysis, forensic reconstruction, and audit
#' workflows.
#'
#' @param df A `data.frame` representing a filesystem snapshot. Must conform
#'   to the canonical schema (see [normalise_snapshot_schema()]), including:
#'   - `rel_path`
#'   - `filename`
#'   - `mtime` (POSIXct)
#'   - `extension`
#'   - optionally `git_tracked`
#' @param extensions Character vector of file extensions to include
#'   (case-insensitive, without leading dots).
#' @param path_col Character. Name of the column containing file paths
#'   (default: `"rel_path"`).
#' @param time_unit One of `"week"`, `"month"`, `"day"`, `"year"`.
#' @param max_files Integer. Maximum number of file names shown per group.
#'
#' @return A `data.frame` with one row per (period × group_path), containing:
#' \describe{
#'   \item{period}{Time bucket identifier (e.g. `"2026-17"`).}
#'   \item{group_path}{Project-level grouping derived from the first components
#'   of `rel_path`, typically representing project and module (e.g.
#'   `_packages/iocodelists/R`).}
#'   \item{start}{Earliest modification date in the group.}
#'   \item{end}{Latest modification date in the group.}
#'   \item{file_names}{Pipe-separated list of filenames (truncated).}
#'   \item{n_files}{Number of file observations in the group.}
#'   \item{n_unique_files}{Number of distinct files (`rel_path`) in the group.}
#'   \item{untracked}{Number of files not tracked by Git (if available).}
#' }
#'
#' @details
#' This function operates on **observational data**:
#'
#' - grouping is structural and deterministic, based on the first components
#'   of `rel_path`, typically corresponding to project and module folders
#'   (e.g. `R`, `tests`, `data-raw`)
#' - no assumptions are made about project structure or file roles
#' - identical inputs always produce identical outputs
#'
#' The `group_path` is a **project–module level projection of `rel_path`**.
#' It is derived by extracting the first components of the path (e.g.
#' `_packages/iocodelists/R`) and is intended for aggregation and reporting.
#'
#' The output is intended for **analysis and reporting**, not for
#' file-level identity or joins. For identity, use `rel_path`.
#'
#' Modification times (`mtime`) are treated as a proxy for activity.
#' They indicate observed changes, not a complete editing history.
#'
#' Files under `.Trash` are excluded by default.

#' This approach aligns grouping with typical project layouts (e.g. R packages),
#' where the first directory levels correspond to project boundaries and
#' functional modules.
#' @examples
#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' summarise_activity(
#'   fscontextdemo_snapshot_02,
#'   time_unit = "month"
#' )
#' @seealso [path_prefix()], [normalise_snapshot_schema()]
#' @importFrom dplyr filter mutate group_by summarise arrange n desc n_distinct
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
summarise_activity <- function(df,
                               extensions = c("r", "bak"),
                               path_col = "rel_path",
                               time_unit = c("week", "month", "day", "year"),
                               max_files = 20) {
  df <- normalise_snapshot_schema(df)

  time_unit <- match.arg(time_unit)
  extensions <- tolower(gsub("^\\.", "", extensions))

  df <- df |>
    dplyr::filter(
      !is.na(extension),
      extension %in% extensions,
      !grepl("^\\.Trash", .data[[path_col]])
    )

  date <- as.Date(df$mtime)

  period <- switch(time_unit,
    week  = format(date, "%Y-%U"),
    month = format(date, "%Y-%m"),
    day   = format(date, "%Y-%m-%d"),
    year  = format(date, "%Y")
  )

  has_git <- "git_tracked" %in% names(df)

  df |>
    dplyr::mutate(
      date = date,
      period = period,
      group_path = derive_group_path(.data[[path_col]])
    ) |>
    dplyr::group_by(period, group_path) |>
    dplyr::summarise(
      start = min(date),
      end = max(date),
      file_names = paste(head(unique(filename), max_files), collapse = " | "),
      n_files = dplyr::n(),
      n_unique_files = dplyr::n_distinct(.data[[path_col]]),
      untracked = if (has_git) {
        sum(git_tracked == FALSE, na.rm = TRUE)
      } else {
        NA_integer_
      },
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(period), group_path)
}


#' @rdname summarise_activity
#' @export
summarize_activity <- summarise_activity
