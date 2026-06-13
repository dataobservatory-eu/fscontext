#' Remove storage context from contextualised filesystem paths
#'
#' Removes the `storage_id::` contextual prefix introduced in snapshot
#' schema version 0.1.3 and returns the corresponding machine-local
#' filesystem path.
#'
#' The helper supports interoperability with functions that require
#' local filesystem paths rather than globally contextualised
#' observational locators.
#'
#' Example of contextualised observational locator:
#'
#' `l460-broken-ssd::C:/_packages/eviota/R/file.R`
#'
#' becomes:
#'
#' `C:/_packages/eviota/R/file.R`
#'
#' Paths without a storage prefix are returned unchanged.
#'
#' @param x Character vector of contextualised filesystem paths.
#'
#' @return
#' Character vector with storage prefixes removed.
#'
#' @examples
#' strip_storage_prefix(
#'   "l460::C:/_packages/eviota/R/file.R"
#' )
#'
#' strip_storage_prefix(
#'   c(
#'     "l460::C:/test.txt",
#'     "l480::D:/data.csv"
#'   )
#' )
#'
#' @keywords internal
#' @noRd
strip_storage_prefix <- function(x) {
  sub("^[^:]+::", "", x)
}
