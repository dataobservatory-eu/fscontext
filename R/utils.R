#' Standardize contextual roots
#'
#' Internal helper that resolves supported contextual
#' boundary declarations into a character vector of
#' normalized contextual roots.
#'
#' Supported inputs include:
#'
#' - character vectors of contextual roots;
#' - context objects containing a `$contexts` element.
#'
#' Context objects are flattened using
#' `flatten_context_roots()`.
#'
#' @param x Character vector of contextual roots or a
#'   context object.
#'
#' @return
#' A character vector of contextual roots.
#'
#' @keywords internal
#' @noRd
standardize_context_roots <- function(x) {
  if (is.character(x)) {
    return(x)
  }

  if (is.list(x)) {
    return(
      flatten_context_roots(x)
    )
  }

  stop(
    paste(
      "roots must be either:",
      "- a character vector of contextual roots;",
      "- a named list of contextual definitions."
    ),
    call. = FALSE
  )
}


#' Flatten contextual roots
#'
#' Internal helper that extracts and flattens
#' contextual roots from a list of contextual
#' definitions.
#'
#' Each context is expected to contain a `roots`
#' element defining contextual boundary roots used
#' for curatorial or analytical aggregation.
#'
#' @param contexts List of contextual definitions.
#'
#' @return
#' A character vector of contextual roots.
#'
#' @importFrom purrr map
#'
#' @keywords internal
#' @noRd
flatten_context_roots <- function(contexts) {
  unlist(
    contexts,
    use.names = FALSE
  )
}


#' Normalize contextual roots
#'
#' Internal helper that normalizes contextual roots
#' for stable contextual boundary comparison.
#'
#' The function:
#'
#' - converts backslashes to forward slashes;
#' - removes trailing slashes.
#'
#' @param x Character vector of contextual roots.
#'
#' @return
#' A normalized character vector of contextual roots.
#'
#' @keywords internal
#' @noRd
normalize_context_roots <- function(x) {
  stopifnot(is.character(x))
  x <- gsub("\\\\", "/", x)
  x <- sub("/+$", "", x)
  x
}

#' Assert uniqueness of contextual roots
#'
#' Internal helper that validates that contextual
#' roots are uniquely declared.
#'
#' Contextual roots define curatorial or analytical
#' aggregation boundaries and therefore must not
#' contain duplicates.
#'
#' @param x Character vector of contextual roots.
#'
#' @return
#' Invisibly returns `TRUE`.
#'
#' @keywords internal
#' @noRd
assert_unique_context_roots <- function(x) {
  stopifnot(is.character(x))

  duplicated_roots <- unique(x[duplicated(x)])

  if (length(duplicated_roots) > 0) {
    stop(
      paste(
        "Duplicated roots:",
        paste(
          duplicated_roots,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Prepare contextual roots
#'
#' Internal helper that standardizes, normalizes,
#' and validates contextual roots before contextual
#' coverage evaluation or Record Set derivation.
#'
#' @param x Character vector of contextual roots or
#'   a context object.
#'
#' @return
#' A normalized character vector of unique
#' contextual roots.
#'
#' @keywords internal
#' @noRd
prepare_context_roots <- function(x) {
  x <- standardize_context_roots(x)
  x <- normalize_context_roots(x)
  assert_unique_context_roots(x)
  x
}

#' Calculate filesystem path depth
#'
#' Internal helper that calculates normalized
#' filesystem path depth for observational or
#' contextual aggregation boundaries.
#'
#' Paths are normalized using `normalize_context_roots()`
#' before depth calculation.
#'
#' Examples:
#'
#' - `"D:"` -> depth 1
#' - `"D:/packages"` -> depth 2
#' - `"D:/packages/eviota"` -> depth 3
#'
#' Trailing slashes and backslashes are normalized
#' before evaluation.
#'
#' @param x Character vector of filesystem paths.
#'
#' @return
#' Integer vector giving normalized filesystem
#' path depth.
#'
#' @importFrom purrr map_int
#'
#' @keywords internal
#' @noRd
path_depth <- function(x) {
  x <- normalize_context_roots(x)

  purrr::map_int(
    x,
    function(p) {
      length(
        strsplit(p, "/")[[1]]
      )
    }
  )
}

#' Calculate aggregation depth
#'
#' Internal helper that derives aggregation depth
#' from normalized filesystem path depth.
#'
#' Aggregation depth is used for scale-aware
#' contextual matching between observational units
#' and contextual aggregation boundaries.
#'
#' @param x Character vector of filesystem paths.
#'
#' @return
#' Integer vector giving aggregation depth.
#'
#' @keywords internal
#' @noRd
aggregation_depth <- function(x) {
  pmax(path_depth(x) - 1, 0)
}

#' Normalize Git remote identifiers
#'
#' Converts Git remotes to a stable comparable form.
#'
#' Normalization currently:
#'
#' - converts GitHub SSH remotes to HTTPS form
#' - removes trailing `.git`
#' - removes trailing `/`
#'
#' @param x Character vector of Git remotes.
#'
#' @return Character vector of normalized Git repository identifiers.
#'
#' @keywords internal
normalize_git_remote <- function(x) {
  x <- sub(
    "^git@github\\.com:",
    "https://github.com/",
    x
  )

  x <- sub("\\.git$", "", x)
  x <- sub("/$", "", x)
  x
}
