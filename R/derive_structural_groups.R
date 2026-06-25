#' Derive structural aggregation metadata from relative paths
#'
#' Derives lightweight structural aggregation metadata from observed
#' filesystem locators.
#'
#' The function identifies shallow structural patterns in relative
#' filesystem paths and creates candidate aggregations that may help
#' users explore, navigate, and contextualise observed resources.
#'
#' The resulting groupings are structural projections derived from
#' path organisation alone. They do not represent authoritative
#' documentary structure, provenance relationships, or Record Set
#' assertions.
#'
#' Structural aggregation metadata can increase the potential
#' informativeness of filesystem observations by exposing recurring
#' organisational patterns that may support later analytical,
#' curatorial, or archival interpretation.
#'
#' @param rel_path Character vector of relative filesystem paths.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{structural_group}{
#'   Candidate aggregation derived from the leading path components.
#'   }
#'   \item{component}{
#'   Immediate structural subdivision within the aggregation,
#'   when present.
#'   }
#' }
#'
#' @details
#' The function operates on observed locators and derives aggregation
#' metadata from structural organisation alone.
#'
#' In the fscontext workflow:
#'
#' - filesystem observations provide evidence about observed resources;
#' - relative paths provide structural information about those resources;
#' - structural aggregation metadata exposes candidate groupings that
#'   may later support contextual reconstruction, exploratory analysis,
#'   semantic stabilisation, or Record Set construction.
#'
#' The resulting groupings are analytical projections rather than
#' semantic assertions. They are intended to help identify potentially
#' informative objects and candidate aggregations, not to establish
#' authoritative documentary relationships.
#'
#' Future versions may introduce alternative aggregation strategies
#' based on other observational evidence such as provenance,
#' authorship, temporal patterns, repository context, or content-based
#' signals.
#'
#' @importFrom dplyr bind_rows
#' @export

derive_structural_groups <- function(
  rel_path,
  profile = "folder-depth-2"
) {
  if (is.null(rel_path) || !is.character(rel_path)) {
    stop(
      "rel_path must be a character vector",
      call. = FALSE
    )
  }

  rel_path <- gsub("\\\\", "/", rel_path)

  parts <- strsplit(rel_path, "/", fixed = TRUE)

  depth <- switch(profile,
    "folder-depth-1" = 1L,
    "folder-depth-2" = 2L,
    "folder-depth-3" = 3L,
    "folder-depth-4" = 4L,
    "wacz" = NA_integer_,
    stop(
      "Unknown profile: ",
      profile,
      call. = FALSE
    )
  )

  res <- lapply(parts, function(p) {
    p <- p[nzchar(p)]

    if (length(p) == 0 || all(is.na(p))) {
      return(list(
        structural_group = NA_character_,
        component = NA_character_
      ))
    }

    if (profile == "wacz") {
      structural_group <- p[1]

      component <- if (length(p) > 1) {
        p[2]
      } else {
        NA_character_
      }
    } else {
      group_depth <- min(depth, length(p))

      structural_group <- paste(
        p[seq_len(group_depth)],
        collapse = "/"
      )

      component <- if (length(p) > group_depth) {
        p[group_depth + 1]
      } else {
        NA_character_
      }
    }

    list(
      structural_group = structural_group,
      component = component
    )
  })

  dplyr::bind_rows(res)
}
