#' Summarise observed activity from filesystem observations
#'
#' Aggregates filesystem observations into reproducible temporal
#' summaries grouped by structural path context.
#'
#' @description
#' `summarise_observed_activity()` summarises observed file-level
#' modification evidence by time period and structural grouping.
#'
#' The function is intended as a temporal contextualisation step.
#' It does not infer archival Activities, Events, Record Sets, or
#' provenance relations. Instead, it produces candidate activity
#' summaries that may support later human review, semantic
#' stabilisation, or RiC-aligned modelling.
#'
#' @details
#' The function derives:
#'
#' - `period`, a time bucket derived from file modification times
#'   (`mtime`);
#' - `group_path`, a structural grouping key derived from the selected
#'   path column.
#'
#' It then summarises observations within each
#' `period` x `group_path` combination.
#'
#' Modification times are treated as observational evidence of change.
#' They are not interpreted as complete editing histories or confirmed
#' archival events.
#'
#' Structural grouping is deterministic and based on path structure.
#' It is intended for aggregation, review, and reporting, not for
#' file identity. Use `rel_path` for file-level identity.
#'
#' Typical uses include:
#'
#' - identifying temporal clusters of filesystem activity;
#' - reviewing candidate Activities before semantic stabilisation;
#' - comparing activity across structural contexts;
#' - supporting archival recontextualisation workflows;
#' - preparing analytical or audit summaries.
#'
#' Files under `.Trash` are excluded.
#'
#' @param df A `data.frame` representing filesystem observations.
#'   The data must contain:
#'
#'   - `rel_path`;
#'   - `filename`;
#'   - `mtime`;
#'   - `extension`.
#'
#'   If present, `git_tracked` is used to count untracked files.
#'
#' @param extensions Character vector of file extensions to include,
#'   without leading dots. Matching is case-insensitive.
#'
#' @param path_col Character scalar. Name of the column containing
#'   paths used to derive structural groupings. Defaults to `"rel_path"`.
#'
#' @param time_unit Character scalar. One of `"week"`, `"month"`,
#'   `"day"`, or `"year"`.
#'
#' @param max_files Integer. Maximum number of file names displayed
#'   in each summary row.
#'
#' @return
#' A `data.frame` with one row per `period` and `group_path`
#' combination.
#'
#' The returned columns include:
#'
#' \describe{
#'   \item{period}{
#'   Time bucket identifier.
#'   }
#'   \item{group_path}{
#'   Structural grouping key derived from `path_col`.
#'   }
#'   \item{start}{
#'   Earliest observed modification date in the group.
#'   }
#'   \item{end}{
#'   Latest observed modification date in the group.
#'   }
#'   \item{file_names}{
#'   Pipe-separated sample of observed file names.
#'   }
#'   \item{n_files}{
#'   Number of file observations in the group.
#'   }
#'   \item{n_unique_files}{
#'   Number of distinct paths in the group.
#'   }
#'   \item{untracked}{
#'   Number of observations not tracked by Git, when `git_tracked`
#'   is available; otherwise `NA_integer_`.
#'   }
#' }
#'
#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' summarise_observed_activity(
#'   fscontextdemo_snapshot_02,
#'   time_unit = "month"
#' )
#'
#' @importFrom dplyr filter mutate group_by summarise arrange n desc n_distinct
#' @importFrom rlang .data
#' @importFrom utils head
#'
#' @export
summarise_observed_activity <- function(
    df,
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


#' @rdname summarise_observed_activity
#' @export
summarize_observed_activity <- summarise_observed_activity
