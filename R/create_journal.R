#' Create a consolidated observational event journal
#'
#' Combines filesystem-derived observational events and
#' optional GitHub-derived events into a unified event journal.
#'
#' The function validates the resulting journal structure and
#' returns a chronologically ordered tibble of observational
#' events.
#'
#' @param filesystem_events A data frame containing
#'   filesystem-derived observational events, typically created
#'   with [project_record_events()].
#'
#' @param github_events Optional data frame containing
#'   GitHub-derived file events, typically created with
#'   [create_github_commit_journal()].
#'
#' @return
#' A tibble containing a consolidated observational
#' event journal.
#'
#' @details
#' The function operates on already projected event tables and
#' does not infer semantic workflows, derivations, or activities.
#' @examples
#' \dontrun{
#'
#' data("fscontextdemo_snapshot_02")
#'
#' snapshot_df <- fscontextdemo_snapshot_02
#'
#' snapshot_df$git_remote <-
#'   "https://github.com/dataobservatory-eu/fscontextdemo"
#'
#' snapshot_df$git_branch <- "main"
#'
#' filesystem_events <-
#'   project_record_events(snapshot_df)
#'
#' github_events <-
#'   create_github_commit_journal(
#'     snapshot_df,
#'     per_page = 5
#'   )
#'
#' journal <-
#'   create_journal(
#'     filesystem_events = filesystem_events,
#'     github_events = github_events
#'   )
#'
#' head(journal)
#' }
#'
#' @export
create_journal <- function(
  filesystem_events,
  github_events = NULL
) {
  if (!inherits(filesystem_events, "data.frame")) {
    stop(
      "filesystem_events must inherit from data.frame",
      call. = FALSE
    )
  }

  if (!is.null(github_events)) {
    if (!inherits(github_events, "data.frame")) {
      stop(
        "github_events must inherit from data.frame",
        call. = FALSE
      )
    }
  }

  observation_time <- Sys.time()

  journal <- dplyr::bind_rows(
    filesystem_events,
    github_events
  ) %>%
    dplyr::arrange(
      .data$event_time
    )

  validate_journal(journal)

  attr(journal, "created_by") <-
    "create_journal"

  attr(journal, "created_at") <-
    observation_time

  journal
}
