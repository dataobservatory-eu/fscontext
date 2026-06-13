#' Derive contextual Record Set membership
#'
#' Derive contextual Record Set membership from filesystem
#' observations using declared contextual boundaries.
#'
#' The function creates a contextual membership layer over an
#' observational universe by assigning observed filesystem
#' units to one or more analytical, operational, or curatorial
#' contexts.
#'
#' Contexts are defined explicitly through boundary roots.
#' Membership is derived using recursive path-prefix matching.
#'
#' This approach is inspired by the Records in Contexts (RiC)
#' conceptual model, where records and instantiations may
#' participate in multiple overlapping contexts rather than
#' belonging exclusively to a single hierarchical structure.
#'
#' In fscontext, a context represents a meaningful analytical
#' or curatorial perspective over observed filesystem units.
#' It is not intended to represent an authoritative archival
#' Record Set or a complete documentary aggregation.
#'
#' The resulting membership layer can support:
#'
#' - contextual reconstruction workflows;
#' - Record Set derivation;
#' - provenance analysis;
#' - filesystem archaeology;
#' - semantic stabilisation workflows.
#'
#' The original observational universe remains unchanged.
#'
#' @param x A data frame containing observational filesystem
#'   units, typically created with [observe_universe()] or
#'   derived from [scan_storage()].
#'
#' @param contextual_groups A data frame containing declared
#'   contextual boundaries.
#'
#'   Required columns:
#'
#'   \describe{
#'     \item{context}{
#'       Identifier of the analytical or curatorial context.
#'     }
#'     \item{root}{
#'       Filesystem boundary used to derive contextual
#'       membership.
#'     }
#'   }
#'
#' @param observed_unit_var Character scalar giving the name
#'   of the observational unit variable.
#'   Defaults to `"observed_unit"`.
#'
#' @param include_subfolders Logical.
#'   Currently retained for future compatibility.
#'   Contextual membership is presently derived using
#'   recursive path-prefix matching.
#'
#' @return
#' A tibble containing observational units assigned to one
#' or more contexts.
#'
#' Additional variables include:
#'
#' \describe{
#'   \item{context}{
#'     Context identifier.
#'   }
#'   \item{context_root}{
#'     Boundary root used for membership derivation.
#'   }
#'   \item{construction_method}{
#'     Membership derivation method.
#'   }
#'   \item{derived_by}{
#'     Function that created the membership assignment.
#'   }
#'   \item{derived_at}{
#'     Timestamp when membership was derived.
#'   }
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
#' toy_groups <- tibble::tibble(
#'   context = "eviota",
#'   root = "D:/projects/eviota"
#' )
#'
#' derive_record_set(
#'   toy_universe,
#'   toy_groups
#' )
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
