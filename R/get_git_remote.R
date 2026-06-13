#' Extract Git remote origin URL from a repository
#'
#' Reads the `.git/config` file of a repository and extracts the
#' `remote.origin.url` value if present.
#'
#' The function provides lightweight repository provenance and
#' dissemination context without invoking Git commands or parsing
#' repository history.
#'
#' @details
#' Remote repository URLs help contextualise observed filesystem
#' Instantiations within distributed development environments:
#'
#' - linking local observations to upstream repositories
#' - identifying likely shared project contexts
#' - distinguishing forks and replicated repositories
#' - supporting later Record Set construction
#'
#' The function performs only lightweight structural extraction:
#'
#' - no Git history is analysed
#' - no repository state is modified
#' - no network access occurs
#'
#' In RiC-aligned operational terms, the remote URL acts as contextual
#' provenance evidence associated with observed filesystem
#' Instantiations and repository environments.
#'
#' Missing or inaccessible repository metadata returns `NA_character_`.
#'
#' @param repo_root Character. Path to repository root.
#'
#' @return Character. Remote URL, or `NA_character_` if not found.
#'
#' @examples
#' \dontrun{
#' get_git_remote("/path/to/repo")
#' }
#'
#' @export

get_git_remote <- function(repo_root) {
  stopifnot(is.character(repo_root), length(repo_root) == 1)

  config <- fs::path(repo_root, ".git", "config")

  if (!file.exists(config)) {
    return(NA_character_)
  }

  lines <- readLines(config, warn = FALSE)

  # --- locate origin remote block ---
  # lightweight repository provenance extraction

  origin_idx <- grep("\\[remote \"origin\"\\]", lines)

  if (length(origin_idx) == 0) {
    return(NA_character_)
  }

  # --- extract remote URL ---
  # inspect only nearby lines to avoid unrelated remotes

  block <- lines[
    origin_idx:min(origin_idx + 5, length(lines))
  ]

  url_line <- grep(
    "url\\s*=\\s*",
    block,
    value = TRUE
  )

  if (length(url_line) == 0) {
    return(NA_character_)
  }

  sub(".*url\\s*=\\s*", "", url_line[1])
}
