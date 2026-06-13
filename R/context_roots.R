#' Extract normalized contextual roots
#'
#' Returns a normalized character vector of contextual
#' roots from either:
#'
#' - a character vector of contextual roots;
#' - a named list of contextual definitions.
#'
#' The function:
#'
#' - flattens contextual roots;
#' - normalizes filesystem separators;
#' - removes trailing separators;
#' - validates uniqueness.
#'
#' This is the public interface for obtaining
#' contextual aggregation boundaries from context
#' definitions.
#'
#' @param x Character vector of contextual roots or
#'   a named list of contextual definitions.
#'
#' @return
#' A normalized character vector of unique
#' contextual roots.
#'
#' @examples
#' mini_context <- list(
#'   packages = c(
#'     "D:/packages/examplepkg",
#'     "C:/packages/examplepkg"
#'   ),
#'   research = "D:/research/projectA"
#' )
#'
#' context_roots(mini_context)
#'
#' @export

context_roots <- function(x) {
  prepare_context_roots(x)
}
