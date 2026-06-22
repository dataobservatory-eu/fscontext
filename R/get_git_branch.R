#' Get current Git branch of repository
#'
#' Reads the `.git/HEAD` file and returns the current branch name,
#' or "DETACHED" if the repository is in detached HEAD state.
#'
#' @param repo_root Character. Path to repository root.
#'
#' @return Character. Branch name, "DETACHED", or NA if not available.
#'
#' @keywords internal
#' @importFrom fs path
#' @noRd

get_git_branch <- function(repo_root) {
  stopifnot(is.character(repo_root), length(repo_root) == 1)

  head_file <- fs::path(repo_root, ".git", "HEAD")

  if (!file.exists(head_file)) {
    return(NA_character_)
  }

  head <- readLines(head_file, warn = FALSE)

  if (length(head) == 0) {
    return(NA_character_)
  }

  if (grepl("^ref:", head[1])) {
    sub("^ref: refs/heads/", "", head[1])
  } else {
    "DETACHED"
  }
}
