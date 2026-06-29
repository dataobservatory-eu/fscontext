#' Derive structural aggregation metadata from relative paths
#'
#' @description
#' Derives lightweight structural aggregation metadata from observed
#' relative filesystem paths.
#'
#' The function identifies recurring structural patterns in directory
#' hierarchies and creates candidate aggregations that can support
#' exploratory analysis, navigation, contextual reconstruction, and
#' later semantic interpretation.
#'
#' The resulting groupings are derived solely from path structure.
#' They are analytical projections rather than authoritative Record
#' Sets, provenance assertions, or documentary relationships.
#'
#' @param rel_path Character vector of relative filesystem paths.
#'
#' @param profile Character scalar specifying the structural
#'   aggregation strategy. Available profiles are:
#'   \describe{
#'   \item{"folder-depth-1"}{Group by the first directory level.}
#'   \item{"folder-depth-2"}{Group by the first two directory levels
#'   (default).}
#'   \item{"folder-depth-3"}{Group by the first three directory levels.}
#'   \item{"folder-depth-4"}{Group by the first four directory levels.}
#'   \item{"wacz"}{Use the first path component as the structural group
#'   and the second component as the structural subdivision, matching
#'   the standard organisation of WACZ archives.}
#'   }
#'
#' @return
#' A `data.frame` with two columns:
#' \describe{
#'   \item{structural_group}{
#'   Candidate structural aggregation derived from the selected path
#'   profile.
#'   }
#'   \item{component}{
#'   Immediate structural subdivision within the aggregation, when
#'   present.
#'   }
#' }
#'
#' @details
#' Structural aggregation metadata provides a lightweight abstraction
#' of observed directory organisation. It can increase the
#' informativeness of filesystem observations by exposing recurring
#' organisational patterns without asserting semantic meaning.
#'
#' Within the fscontext workflow:
#'
#' * filesystem observations provide evidence about observed resources;
#' * relative paths provide structural organisation;
#' * structural aggregations expose candidate groups that may later
#'   support contextual reconstruction, Record Set construction,
#'   semantic stabilisation, or other downstream analyses.
#'
#' Future versions may introduce additional aggregation profiles based
#' on repository structure, provenance, temporal patterns, or other
#' observational evidence.
#' @examples
#' rel_path <- c(
#'   "_packages/demo/R/file.R",
#'   "_packages/demo/tests/testthat/test-file.R",
#'   "_packages/demo/data/input.csv"
#' )
#'
#' derive_structural_groups(rel_path)
#'
#' derive_structural_groups(
#'   rel_path,
#'   profile = "folder-depth-1"
#' )
#'
#' derive_structural_groups(
#'   c(
#'     "archive/data.warc.gz",
#'     "indexes/index.cdx",
#'     "pages/pages.jsonl"
#'   ),
#'   profile = "wacz"
#' )
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
