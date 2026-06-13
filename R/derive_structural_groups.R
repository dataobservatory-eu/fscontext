#' Derive structural grouping heuristics from relative paths
#'
#' Derives lightweight structural grouping heuristics from relative
#' filesystem paths.
#'
#' The function extracts shallow structural patterns commonly found in
#' software projects, research workflows, and digital working environments.
#'
#' It assigns:
#'
#' - `structural_group`:
#'   grouping heuristic derived from the first path component
#'   (e.g. `innolab25`, `_packages`, `_markdown`)
#'
#' - `component`:
#'   immediate structural subdivision within the grouping,
#'   if present
#'   (e.g. `eviota`, `filmledgerimport`, `iotables`)
#'
#' These derived structures support:
#'
#' - exploratory grouping of filesystem observations
#' - navigation of large observational snapshots
#' - reconstruction of operational project environments
#' - identification of candidate documentary aggregations
#'
#' The function performs deterministic structural projection only.
#' It does not validate repository semantics, documentary structure,
#' or authoritative Record Set boundaries.
#'
#' @param rel_path Character vector of relative filesystem paths.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{structural_group}{
#'   Filesystem-based structural grouping heuristic derived from
#'   the first path component.
#'   }
#'   \item{component}{
#'   Immediate structural subdivision within the grouping,
#'   if present.
#'   }
#' }
#'
#' @details
#' This function provides a lightweight structural interpretation layer
#' on top of observational filesystem data.
#'
#' In RiC-aligned operational terms:
#'
#' - rows in observational snapshots represent filesystem
#'   Instantiations
#'
#' - `rel_path` acts as an operational locator associated with
#'   observed filesystem occurrences
#'
#' - the derived structural groupings provide analytical heuristics
#'   that may later support Record Set construction
#'
#' The derived groupings are operational analytical projections,
#' not authoritative RiC Record Sets.
#'
#' The function is intended for analytical, navigational,
#' and exploratory reconstruction workflows.
#'
#' Future versions of the package may replace or extend this logic with
#' more explicit provenance-aware Record Set construction workflows
#' (for example via `create_record_set()`).

#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' example_paths <- c(
#'   "_packages/fscontextdemo/R/derive_fsdemo_country_data.R",
#'   "_packages/fscontextdemo/tests/testthat/test-country-data.R",
#'   "_packages/fscontextdemo/data-raw/create_fsdemo_country_data.R",
#'   "_packages/fscontextdemo/docs/index.html"
#' )
#'
#' data.frame(
#'   rel_path = example_paths,
#'   derive_structural_groups(example_paths)
#' )
#'
#' @importFrom dplyr bind_rows
#' @export

derive_structural_groups <- function(rel_path) {
  parts <- strsplit(rel_path, "/", fixed = TRUE)

  res <- lapply(parts, function(p) {
    p <- p[nzchar(p)]

    if (length(p) == 0) {
      return(list(
        structural_group = NA_character_,
        component = NA_character_
      ))
    }

    if (length(p) == 1) {
      return(list(
        structural_group = p[1],
        component = NA_character_
      ))
    }

    if (length(p) == 2) {
      return(list(
        structural_group = paste(p[1], p[2], sep = "/"),
        component = NA_character_
      ))
    }

    structural_group <- paste(
      p[1],
      p[2],
      sep = "/"
    )

    component <- p[3]

    list(
      structural_group = structural_group,
      component = component
    )
  })

  dplyr::bind_rows(res)
}
