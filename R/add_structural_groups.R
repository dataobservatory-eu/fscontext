#' Attach structural grouping heuristics to snapshot observations
#'
#' Adds lightweight structural grouping variables derived from
#' filesystem path structure.
#'
#' The function appends deterministic analytical grouping projections
#' based on `rel_path`, helping organise observational filesystem
#' Instantiations into operationally meaningful structural clusters.
#'
#' @param df A `data.frame` containing a `rel_path` column.
#'
#' @return The input `data.frame` with additional columns:
#' \describe{
#'   \item{structural_group}{
#'   Filesystem-based structural grouping heuristic derived from the
#'   first two path components.
#'   }
#'   \item{component}{
#'   Immediate structural subdivision within the grouping.
#'   }
#' }
#'
#' @details
#' This is a convenience wrapper around [derive_structural_groups()]
#' for use in analytical and reconstruction workflows.
#'
#' The function does not construct authoritative RiC Record Sets.
#'
#' Instead, it derives lightweight structural grouping heuristics that may:
#'
#' - support exploratory analysis
#' - help identify operational project boundaries
#' - assist reconstruction of distributed working environments
#' - provide candidate structures for later Record Set construction
#'
#' The derived groupings reflect filesystem organisation rather than
#' authoritative documentary arrangement or curatorial interpretation.
#'
#' In RiC-aligned operational terms:
#'
#' - rows in observational snapshots represent filesystem
#'   Instantiations
#'
#' - `rel_path` acts as an operational locator associated with
#'   observed filesystem occurrences
#'
#' - `structural_group` and `component` provide deterministic
#'   structural grouping heuristics that may later support
#'   provenance-aware Record Set construction
#'
#' Future versions of the package may replace or extend this logic
#' with more explicit provenance-aware Record Set construction workflows
#' (for example via `create_record_set()`).
#'
#' @seealso [derive_structural_groups()]
#'
#' @importFrom dplyr bind_cols
#' @export

add_structural_groups <- function(df) {
  if (!"rel_path" %in% names(df)) {
    stop("Column 'rel_path' is required")
  }

  rs <- derive_structural_groups(df$rel_path)

  dplyr::bind_cols(df, rs)
}
