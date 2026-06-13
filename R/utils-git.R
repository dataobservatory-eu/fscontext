#' @importFrom dplyr filter slice_head
#' @keywords internal
#' @noRd
commit_select <- function(
  commit_index,
  max_commits = 100,
  since = NULL
) {
  stopifnot(is.data.frame(commit_index))

  if (nrow(commit_index) <= max_commits) {
    return(commit_index)
  }

  if (!is.null(since)) {
    return(
      commit_index |>
        dplyr::filter(
          commit_time >= as.POSIXct(since, tz = "UTC")
        )
    )
  }

  commit_index |>
    dplyr::slice_head(n = max_commits)
}
