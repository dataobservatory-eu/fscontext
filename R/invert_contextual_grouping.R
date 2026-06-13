#' Invert contextual grouping mappings into relational form
#'
#' Converts grouped contextual mappings into a long-form relational
#' representation.
#'
#' `invert_contextual_grouping()` is useful for transforming
#' lightweight contextual grouping structures into tidy relational
#' tables suitable for:
#'
#' - joins;
#' - contextual reconstruction;
#' - Record Set projections;
#' - provenance-aware grouping workflows;
#' - semantic enrichment pipelines;
#' - lightweight graph construction.
#'
#' The function is intentionally operational and lightweight.
#'
#' It does not:
#'
#' - enforce uniqueness;
#' - validate ontology semantics;
#' - construct graph objects;
#' - distinguish authoritative from analytical groupings;
#' - or infer hierarchical relations.
#'
#' @param x A named list containing contextual grouping mappings.
#'
#' Each list name represents a contextual grouping and each list
#' element contains one or more associated resources.
#'
#' @return
#' A tibble with two columns:
#'
#' \describe{
#'   \item{member}{
#'   Contextually grouped resource.
#'   }
#'   \item{group}{
#'   Contextual grouping identifier.
#'   }
#' }
#'
#' @details
#' The function is conceptually related to [as_value_key()] but
#' produces a relational projection rather than a canonical named
#' vector representation.
#'
#' This is particularly useful for one-to-many contextual mappings,
#' where multiple resources belong to the same contextual grouping.
#'
#' The resulting relational representation may later support:
#'
#' - contextual Record Set construction;
#' - semantic overlay workflows;
#' - lightweight provenance analysis;
#' - many-to-many reconstruction logic.
#'
#' @examples
#'
#' record_sets <- list(
#'   conceptualisation = c(
#'     "D:/_package/alpha",
#'     "D:/_markdown/alpha-methodology"
#'   ),
#'   betaR = c(
#'     "D:/_packages/beta",
#'     "D:/_packages/prebeta"
#'   )
#' )
#'
#' invert_contextual_grouping(record_sets)
#'
#' # canonical roundtrip
#' as_value_key(
#'   invert_contextual_grouping(record_sets)
#' )
#'
#' @export
invert_contextual_grouping <- function(x) {
  purrr::imap_dfr(
    x,
    ~ tibble::tibble(
      group = .y,
      member = .x
    )
  )
}
