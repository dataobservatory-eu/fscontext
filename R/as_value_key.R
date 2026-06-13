#' Standardize contextual semantic mappings
#'
#' Convert contextual semantic mapping carriers into canonical
#' key-value representations suitable for:
#'
#' - contextual reconstruction;
#' - Record Set projection;
#' - workflow annotation;
#' - semantic stabilisation;
#' - lightweight semantic harmonisation.
#'
#' `as_value_key()` standardizes:
#'
#' - named vectors;
#' - named lists;
#' - two-column tibbles or data frames.
#'
#' Named lists may represent one-to-many contextual mappings,
#' allowing multiple contextual roots to share a semantic
#' grouping.
#'
#' The resulting object is a named character vector compatible
#' with:
#'
#' - [prelabel()];
#' - contextual semantic overlays;
#' - refinement workflows;
#' - lightweight contextual harmonisation.
#'
#' @inheritParams dataset::as_value_key
#'
#' @return
#' A named character vector representing canonical contextual
#' semantic mappings.
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
#' as_value_key(record_sets)
#'
#' invert_value_key(record_sets)
#'
#' @seealso
#' [dataset::as_value_key()]
#'
#' @name as_value_key
#' @export
#' @importFrom dataset as_value_key
dataset::as_value_key

#' Invert contextual semantic mappings
#'
#' Convert contextual semantic mappings into a two-column
#' relational representation suitable for:
#'
#' - joins;
#' - contextual grouping workflows;
#' - Record Set inspection;
#' - relational projection;
#' - roundtrip conversion with [as_value_key()].
#'
#' @inheritParams dataset::invert_value_key
#'
#' @return
#' A two-column tibble containing contextual semantic mappings.
#'
#' @examples
#'
#' record_sets <- list(
#'   conceptualisation = c(
#'     "D:/_package/alpha",
#'     "D:/_markdown/alpha-methodology"
#'   )
#' )
#'
#' invert_value_key(record_sets)
#'
#' @seealso
#' [as_value_key()]
#' [dataset::invert_value_key()]
#'
#' @name invert_value_key
#' @export
#' @importFrom dataset invert_value_key
dataset::invert_value_key
