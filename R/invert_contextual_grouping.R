#' Invert contextual grouping mappings
#'
#' Convert a named list of contextual groupings into a long-form
#' relational table.
#'
#' @description
#' `invert_contextual_grouping()` transforms lightweight contextual
#' grouping definitions into a two-column table of group membership.
#'
#' This is useful when contextual roots, resources, or candidate
#' Record Set members are first declared as a named list but later
#' need to be used in joins, rulebooks, or reconstruction workflows.
#'
#' @param x A named list.
#'   Each list name identifies a contextual group, and each list
#'   element contains one or more members of that group.
#'
#' @return
#' A tibble with two columns:
#'
#' \describe{
#'   \item{group}{
#'   Contextual grouping identifier.
#'   }
#'   \item{member}{
#'   Member associated with the contextual group.
#'   }
#' }
#'
#' @details
#' The function performs a lightweight structural transformation.
#'
#' It does not validate ontology semantics, enforce uniqueness,
#' construct graph objects, or infer hierarchical relations.
#'
#' The result is suitable for relational operations such as joins,
#' filtering, contextual reconstruction, and candidate Record Set
#' membership workflows.
#'
#' @examples
#' record_sets <- list(
#'   conceptualisation = c(
#'     "D:/_packages/alpha",
#'     "D:/_markdown/alpha-methodology"
#'   ),
#'   beta = c(
#'     "D:/_packages/beta",
#'     "D:/_packages/prebeta"
#'   )
#' )
#'
#' invert_contextual_grouping(record_sets)
#'
#' @seealso [as_value_key()], [invert_value_key()]
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
