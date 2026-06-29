#' Derive project-level grouping from relative paths
#'
#' Extracts a deterministic grouping key from `rel_path` by combining
#' the first path components: usually collection/project, optionally module.
#'
#' @details
#' This function implements a lightweight operational grouping heuristic
#' based on shallow relative path structure.
#'
#' It is currently used for analytical navigation, summarisation,
#' and exploratory grouping of observed filesystem Instantiations.
#'
#' The grouping logic reflects practical development and repository
#' layouts rather than authoritative documentary structure.
#'
#' Future versions of the package are expected to replace or absorb
#' this functionality into higher-level Record Set construction logic
#' (for example via `record_set_projection()`), where grouping rules will
#' be explicitly contextualised and provenance-aware.
#' @param rel_path Character vector of relative file paths.
#' @param repo_root Optional. Currently unused.
#'
#' @return Character vector of grouping keys.
#' @keywords internal
derive_group_path <- function(rel_path, repo_root = NULL) {
  rel_path <- gsub("\\\\", "/", rel_path)

  parts <- strsplit(rel_path, "/", fixed = TRUE)

  vapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    p <- p[nzchar(p)]

    if (length(p) == 0) {
      return(NA_character_)
    }

    if (length(p) == 1) {
      return(NA_character_)
    }

    project <- paste(p[1], p[2], sep = "/")

    if (length(p) < 3) {
      return(project)
    }

    third <- p[3]

    # ---- improved file detection ----
    is_file_like <- grepl("\\.", third) ||
      toupper(third) %in% c("DESCRIPTION", "LICENSE", "README", "NAMESPACE")

    if (is_file_like) {
      return(project)
    }

    paste(project, third, sep = "/")
  }, character(1))
}
