#' Create a prelabelled vector
#'
#' Attach provisional semantic assertions to an observational
#' vector.
#'
#' `prelabel()` creates lightweight semantic mappings that support
#' iterative semantic refinement workflows before values mature
#' into formally defined variables created with
#' [labelled::labelled()] or [dataset::defined()].
#'
#' Unlike strictly defined semantic vectors, `prelabelled`
#' vectors tolerate:
#'
#' - incomplete semantic mappings;
#' - unresolved observational values;
#' - contextual ambiguity;
#' - incremental semantic stabilisation.
#'
#' This design is particularly useful in provenance-aware,
#' contextual reconstruction, and archival workflows where
#' semantic interpretations emerge gradually through iterative
#' refinement.
#'
#' The class supports workflows inspired by RiC-O and PROV-O,
#' where observational evidence and semantic interpretation
#' remain explicitly separated.
#'
#' @param x A vector.
#'
#' @param labels Candidate semantic mappings describing
#' provisional semantic assertions.
#'
#' `labels` is internally normalised with [as_value_key()] and
#' may therefore be supplied as:
#'
#' - a named vector;
#' - a named list;
#' - a two-column data frame or tibble.
#'
#' @param unmatched Behaviour for unmatched observational values.
#'
#' One of:
#'
#' - `"keep"` (default): preserve unmatched values as
#'   observational semantic assertions;
#' - `"na"`: operationalise unmatched values as `NA` during
#'   semantic coercion.
#'
#' @param missing_label Semantic assertion used internally for
#' missing observational values.
#'
#' @return
#' A vector with:
#'
#' - class `"prelabelled"`;
#' - attached provisional semantic vocabulary stored in the
#'   `"prelabel"` attribute.
#'
#' @details
#' `prelabelled` vectors intentionally separate:
#'
#' - observational evidence;
#' - semantic operationalisation;
#' - contextual semantic refinement.
#'
#' Original observational values remain unchanged while semantic
#' assertions may evolve through iterative refinement workflows.
#'
#' Semantic operationalisation is available through:
#'
#' - [as.character()] for lightweight semantic coercion;
#' - [as_character()] for provenance-preserving semantic
#'   workspaces.
#'
#' @examples
#'
#' x <- c(
#'   "R",
#'   "png",
#'   "csv",
#'   "unknown"
#' )
#'
#' extension_map <- c(
#'   R = "functional_programming",
#'   png = "visualisation",
#'   csv = "tabular_data"
#' )
#'
#' x <- prelabel(
#'   x,
#'   labels = extension_map
#' )
#'
#' x
#'
#' as.character(x)
#'
#' semantic_workspace <- as_character(x)
#'
#' attributes(semantic_workspace)
#'
#' @seealso
#' [dataset::prelabel()],
#' [dataset::defined()],
#' [dataset::as_value_key()]
#'
#' @name prelabel
#' @export
#' @importFrom dataset prelabel
dataset::prelabel


#' Test if a vector is prelabelled
#'
#' Determine whether an object inherits from the
#' `"prelabelled"` class.
#'
#' Useful for lightweight semantic stabilization and contextual
#' reconstruction workflows where observational evidence and
#' semantic assertions remain explicitly separated.
#'
#' @param x An object.
#'
#' @return
#' Logical scalar.
#'
#' @seealso
#' [prelabel()],
#' [dataset::is.prelabelled()]
#'
#' @name is.prelabelled
#' @export
#' @importFrom dataset is.prelabelled
dataset::is.prelabelled

#' Semantic character coercion
#'
#' Convert objects into semantic character representations.
#'
#' `as_character()` creates operational semantic workspaces
#' suitable for:
#'
#' - contextual refinement;
#' - semantic stabilization;
#' - provenance-aware harmonisation;
#' - contextual reconstruction workflows.
#'
#' Unlike base [as.character()], semantic operationalisation
#' preserves observational provenance and semantic vocabulary.
#'
#' @param x An object.
#' @param ... Additional arguments.
#'
#' @return
#' A semantic operationalisation of `x`.
#'
#' @seealso
#' [prelabel()],
#' [refine()],
#' [dataset::as_character()]
#'
#' @name as_character
#' @export
#' @importFrom dataset as_character
dataset::as_character
