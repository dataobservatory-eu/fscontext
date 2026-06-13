#' Derive contextual Record Set membership
#'
#' Derive contextual Record Set membership from an
#' observational universe using explicit contextual
#' boundary declarations.
#'
#' The function performs a curatorial aggregation step
#' inspired by the "Records in Contexts" (RiC) conceptual
#' model. It derives contextualized Record Set views over
#' an observational universe by assigning observed records
#' or instantiations to one or more meaningful analytical,
#' operational, or curatorial contexts.
#'
#' In this implementation, a `context` does not represent
#' a formally exhaustive or deterministic archival subset.
#' Instead, it represents a meaningful curatorial or
#' analytical perspective over observed records.
#'
#' The same observed records may participate in multiple
#' overlapping contexts simultaneously. This reflects the
#' RiC move away from strict single-hierarchy archival
#' description toward contextual, relational, and
#' many-to-many representations of records.
#'
#' Contextual membership is currently derived using
#' recursive path-prefix inclusion heuristics based on
#' declared contextual boundary roots.
#'
#' This operation does not modify the original
#' observational universe. It creates a contextual
#' membership layer suitable for analytical, archival,
#' provenance, or reporting workflows.
#'
#' @param x An observational universe, typically created
#'   with `scan_storage()` or `observe_universe()`.
#'
#' @param contextual_groups A data frame containing:
#'   \describe{
#'     \item{context}{
#'       Identifier of the curatorial or analytical context.
#'     }
#'     \item{root}{
#'       Observed boundary root used for contextual
#'       membership derivation.
#'     }
#'   }
#'
#' @param observed_unit_var Name of the observational
#'   boundary variable. Defaults to `"observed_unit"`.
#'
#' @param include_subfolders Logical. If `TRUE`
#'   (default), recursively include descendants of
#'   each contextual root.
#'
#' @return
#' A tibble containing derived contextual Record Set
#' membership.
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
#' toy_groups <- tibble::tibble(
#'   context = "eviota",
#'   root = "D:/projects/eviota"
#' )
#'
#' derive_record_set(
#'   toy_universe,
#'   toy_groups
#' )
#' @importFrom dplyr filter mutate
#' @importFrom purrr pmap_dfr map_lgl
#' @importFrom stringr str_replace_all str_replace
#' @importFrom stringr str_starts
#' @importFrom rlang .data
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
