#' Derive contextual Record Set membership
#'
#' Assign observational units to one or more contexts using
#' declared filesystem boundaries.
#'
#' derive_record_set() creates a contextual membership layer
#' over an observational universe. Membership is derived by
#' matching observational units against one or more declared
#' context roots.
#'
#' The function is intended as an intermediate step between
#' filesystem observation and Record Set construction.
#'
#' In a software repository, contexts may correspond to
#' projects, packages, or reporting workflows. In archival
#' environments, contexts may correspond to collections,
#' fonds, or other documentary aggregations.
#'
#' Membership is currently derived using recursive path-prefix
#' matching.
#'
#' @param x A data frame containing observational units,
#' typically created with [observe_universe()].
#'
#' @param contextual_groups A data frame defining contextual
#' boundaries.
#'
#' Must contain:
#'
#' \describe{
#' \item{context}{Context identifier.}
#' \item{root}{Filesystem root used to derive membership.}
#' }
#'
#' @param observed_unit_var Name of the column containing
#' observational units. Defaults to "observed_unit".
#'
#' @param include_subfolders Logical. Currently retained for
#' future compatibility. Membership is derived recursively.
#'
#' @return
#' A tibble containing observational units assigned to one
#' or more contexts.
#'
#' Additional variables include:
#'
#' \describe{
#' \item{context}{Context identifier.}
#' \item{context_root}{Root used for membership derivation.}
#' \item{construction_method}{Membership derivation method.}
#' \item{derived_by}{Function that created the assignment.}
#' \item{derived_at}{Timestamp of derivation.}
#' }
#'
#' @examples
#' toy_universe <- tibble::tibble(
#'   observed_unit = c(
#'     "D:/projects/eviota",
#'     "D:/projects/eviota/tests",
#'     "D:/other"
#'   ),
#'   inst_id = c("a", "b", "c")
#' )
#'
#' contextual_groups <- tibble::tibble(
#'   context = "eviota",
#'   root = "D:/projects/eviota"
#' )
#'
#' derive_record_set(
#'   toy_universe,
#'   contextual_groups
#' )
#'
#' @export
derive_record_set <- function(
  x,
  contextual_groups,
  observed_unit_var = "observed_unit",
  include_subfolders = TRUE
) {
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(contextual_groups))

  required_group_cols <- c(
    "context",
    "root"
  )

  missing_group_cols <- setdiff(
    required_group_cols,
    names(contextual_groups)
  )

  if (length(missing_group_cols) > 0) {
    stop(
      "Missing required columns in contextual_groups: ",
      paste(missing_group_cols, collapse = ", ")
    )
  }

  if (!observed_unit_var %in% names(x)) {
    stop(
      "Missing observational unit variable: ",
      observed_unit_var
    )
  }

  x <- x |>
    dplyr::mutate(
      tmp_observed_unit =
        normalize_context_roots(
          .data[[observed_unit_var]]
        )
    )

  contextual_groups <- contextual_groups |>
    dplyr::mutate(
      root =
        normalize_context_roots(root)
    )

  membership <- purrr::pmap_dfr(
    contextual_groups,
    function(context, root) {
      members <- x |>
        dplyr::filter(
          matches_context_root(
            x = tmp_observed_unit,
            roots = root
          )
        )

      members |>
        dplyr::mutate(
          context = context,
          context_root = root,
          construction_method = "path_prefix",
          derived_by = "derive_record_set",
          derived_at = Sys.time()
        ) |>
        dplyr::select(
          -tmp_observed_unit
        )
    }
  )

  membership
}
