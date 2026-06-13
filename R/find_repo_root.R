#' Find nearest Git repository root for an observed path
#'
#' Given a filesystem path and a set of known Git repository roots,
#' returns the nearest (deepest) matching repository root.
#'
#' The function provides lightweight repository contextualisation for
#' observed filesystem Instantiations without querying Git history
#' or repository internals.
#'
#' @details
#' Repository membership provides important operational and provenance
#' context for later analytical interpretation:
#'
#' - grouping related filesystem observations
#' - identifying software development contexts
#' - distinguishing overlapping project structures
#' - supporting later Record Set construction
#'
#' The function performs purely structural matching:
#'
#' - no Git metadata is modified
#' - no commit history is analysed
#' - no repository semantics are interpreted
#'
#' In RiC-aligned operational terms, the repository root acts as
#' contextual evidence that observed filesystem Instantiations may
#' participate in a shared documentary or development environment.
#'
#' When multiple repository roots match, the deepest matching root
#' is selected.
#'
#' @param path Character. Observed filesystem path.
#' @param repo_roots Character vector of repository root paths.
#'
#' @return Character. Matching repository root, or `NA_character_`
#'   if no repository context is detected.
#'
#' @keywords internal

find_repo_root <- function(path, repo_roots) {
  stopifnot(is.character(path), length(path) == 1)

  stopifnot(is.character(repo_roots))

  # --- structural repository contextualisation ---
  # identifies the nearest repository environment associated
  # with an observed filesystem Instantiation

  matches <- repo_roots[
    vapply(repo_roots, function(r) {
      startsWith(path, paste0(r, .Platform$file.sep)) ||
        identical(path, r)
    }, logical(1))
  ]

  if (length(matches) == 0) {
    return(NA_character_)
  }

  # --- deepest contextual match ---
  # nested repositories inherit the nearest repository context

  matches[which.max(nchar(matches))]
}
