#' Create a GitHub event journal from snapshot metadata
#'
#' Retrieves file-level change events from GitHub repositories
#' referenced in a filesystem snapshot.
#'
#' The function identifies unique GitHub repository and branch
#' combinations from the snapshot and queries the GitHub API
#' for commit and changed-file history.
#'
#' The resulting journal contains observed GitHub file events
#' such as additions, deletions, and modifications.
#'
#' @param snapshot A data frame containing at least
#'   `git_remote` and `git_branch` columns.
#'
#' @param since Optional character or datetime value passed
#'   to the GitHub API `since` parameter to restrict commits
#'   by time.
#'
#' @param per_page Number of commits retrieved per repository.
#'   Defaults to `100`.
#'
#' @return
#' A tibble containing GitHub-derived file events.
#'
#' @details
#' The function operates on observational snapshot metadata
#' and reconstructs file-level events from associated
#' GitHub repositories.
#'
#' Event types currently include:
#'
#' - `"git_add"`
#' - `"git_delete"`
#' - `"git_change"`
#'
#' Repository identifiers are normalized with
#' `normalize_git_remote()`.
#'
#' If no repositories are found, an empty commit journal is
#' returned.
#'
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
#' github_journal <-
#'   create_github_commit_journal(
#'     snapshot_df,
#'     per_page = 5
#'   )
#'
#' head(github_journal)
#' }
#'
#' @importFrom dplyr case_when distinct filter mutate
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd_hms year month day
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export

create_github_commit_journal <- function(
  snapshot,
  since = NULL,
  per_page = 100
) {
  if (!inherits(snapshot, "data.frame")) {
    stop(
      "snapshot must be an object inherited from data.frame",
      call. = FALSE
    )
  }

  required_cols <- c(
    "git_remote",
    "git_branch"
  )

  missing_cols <- setdiff(
    required_cols,
    names(snapshot)
  )

  if (length(missing_cols) > 0) {
    stop(
      "snapshot is missing the following required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  git_repos <- snapshot %>%
    dplyr::filter(
      !is.na(git_remote),
      !is.na(git_branch)
    ) %>%
    dplyr::mutate(
      git_remote =
        normalize_git_remote(git_remote)
    ) %>%
    dplyr::distinct(
      git_remote,
      git_branch
    )

  if (nrow(git_repos) == 0) {
    warning(
      "No GitHub repositories found in snapshot.",
      call. = FALSE
    )

    return(
      empty_commit_journal()
    )
  }

  observation_time <- Sys.time()

  n_repos <- nrow(git_repos)

  pb_repos <- utils::txtProgressBar(
    min = 0,
    max = n_repos,
    style = 3
  )

  on.exit(
    close(pb_repos),
    add = TRUE
  )

  out <- purrr::map_dfr(
    seq_len(n_repos),
    function(i) {
      utils::setTxtProgressBar(
        # outer loop progress bar, number of repos involved
        pb_repos,
        i
      )

      git_remote <-
        git_repos$git_remote[i]

      git_branch <-
        git_repos$git_branch[i]

      repo <- sub(
        "^https://github.com/",
        "",
        git_remote
      )

      repo <- sub(
        "\\.git$",
        "",
        repo
      )


      commits <- tryCatch(
        gh::gh(
          "/repos/{repo}/commits",
          repo = repo,
          sha = git_branch,
          since = since,
          per_page = per_page
        ),
        error = function(e) {
          warning(
            paste0(
              "Failed to retrieve commits for ",
              git_remote,
              " [",
              git_branch,
              "]: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )

          return(NULL)
        }
      )

      if (is.null(commits)) {
        return(empty_commit_journal())
      }

      message(
        "[", i, "/", n_repos, "] ",
        git_remote, " [", git_branch, "] ",
        length(commits), " commits"
      )


      pb_commits <- utils::txtProgressBar(
        min = 0,
        max = length(commits),
        style = 3
      )

      on.exit(
        close(pb_commits),
        add = TRUE
      )

      purrr::map_dfr(
        seq_along(commits),
        function(j) {
          utils::setTxtProgressBar(
            pb_commits,
            j
          )

          commit_obj <- commits[[j]]

          commit_id <- commit_obj$sha

          commit_time <-
            lubridate::ymd_hms(
              commit_obj$commit$author$date,
              tz = "UTC"
            )

          commit_detail <- tryCatch(
            gh::gh(
              "/repos/{repo}/commits/{commit_id}",
              repo = repo,
              commit_id = commit_id
            ),
            error = function(e) {
              warning(
                paste0(
                  "Failed to retrieve commit detail for ",
                  commit_id
                ),
                call. = FALSE
              )

              return(NULL)
            }
          )

          if (is.null(commit_detail)) {
            return(empty_commit_journal())
          }

          purrr::map_dfr(
            commit_detail$files,
            function(f) {
              event_type <- dplyr::case_when(
                f$status == "added" ~ "git_add",
                f$status == "removed" ~ "git_delete",
                TRUE ~ "git_change"
              )

              tibble::tibble(
                time_type =
                  "github_commit",
                event_time =
                  commit_time,
                event_date =
                  as.Date(commit_time),
                year =
                  lubridate::year(commit_time),
                month =
                  lubridate::month(commit_time),
                day =
                  lubridate::day(commit_time),
                observation_time =
                  observation_time,
                event_type =
                  event_type,
                event_source =
                  "github",
                event_evidence =
                  "github_commit",
                event_actor =
                  commit_obj$commit$author$name,
                resource_name =
                  basename(f$filename),
                resource_id =
                  paste0(
                    git_remote,
                    "::",
                    f$filename
                  ),
                git_remote =
                  git_remote,
                git_branch =
                  git_branch,
                repo_rel_path =
                  f$filename,
                filename =
                  basename(f$filename),
                extension =
                  tools::file_ext(f$filename),
                commit_id =
                  commit_id,
                commit_message =
                  commit_obj$commit$message
              )
            }
          )
        }
      )
    }
  )

  attr(out, "created_by") <-
    "create_github_commit_journal"

  attr(out, "created_at") <-
    observation_time

  out
}

#' @keywords internal
#' @importFrom tibble tibble
#' @keywords internal
#' @importFrom tibble tibble
empty_commit_journal <- function() {
  tibble::tibble(
    event_time =
      as.POSIXct(character()),
    event_date =
      as.Date(character()),
    year =
      integer(),
    month =
      integer(),
    day =
      integer(),
    observation_time =
      as.POSIXct(character()),
    event_type =
      character(),
    event_source =
      character(),
    event_evidence =
      character(),
    event_actor =
      character(),
    resource_name =
      character(),
    resource_id =
      character(),
    git_remote =
      character(),
    git_branch =
      character(),
    repo_rel_path =
      character(),
    filename =
      character(),
    extension =
      character(),
    commit_id =
      character(),
    commit_message =
      character()
  )
}
