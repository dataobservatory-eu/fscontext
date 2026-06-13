#' Match contextual roots
#'
#' Internal helper that evaluates whether observational
#' units are included within one or more contextual
#' aggregation boundaries.
#'
#' Matching is performed using normalized recursive
#' path-prefix inclusion semantics.
#'
#' @param x Character vector of observational units.
#'
#' @param roots Character vector of contextual roots.
#'
#' @return
#' Logical vector indicating contextual inclusion.
#'
#' @keywords internal
#' @noRd
matches_context_root <- function(x, roots) {
  x <- normalize_context_roots(x)

  roots <- normalize_context_roots(roots)

  purrr::map_lgl(
    x,
    \(path)
    any(
      path == roots |
        stringr::str_starts(
          path,
          paste0(roots, "/")
        )
    )
  )
}
