#' Evaluate contextual root coverage
#'
#' Evaluates whether observational aggregation units are
#' included in a set of contextual roots.
#'
#' The function operates on observational universes created by
#' [observe_universe()] and performs scale-aware matching between:
#'
#' - observational aggregation units;
#' - contextual roots.
#'
#' Matching is performed using:
#'
#' - normalized filesystem paths;
#' - aggregation depth.
#'
#' Contextual roots and observational units are therefore only
#' matched within the same aggregation depth.
#'
#' The function does not compute coverage summaries or percentages.
#' It only classifies observational units as included or excluded
#' relative to contextual roots.
#'
#' @param provenance Output from [observe_universe()].
#'
#' @param roots Character vector of contextual roots or a
#'   context object.
#'
#' @return
#' A tibble containing observational aggregation units with
#' contextual inclusion status.
#'
#' @importFrom dplyr mutate left_join arrange desc relocate
#' @importFrom tibble tibble
#'
#' @export

coverage_roots <- function(
  provenance,
  roots
) {
  # ------------------------------------------------------------
  # Validate provenance
  # ------------------------------------------------------------

  required_cols <- c(
    "observed_unit",
    "aggregation_depth"
  )

  missing_cols <- setdiff(
    required_cols,
    names(provenance)
  )

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Prepare contextual roots
  # ------------------------------------------------------------

  roots <- prepare_context_roots(roots)

  roots_tbl <- tibble::tibble(
    observed_unit = roots,
    included = TRUE
  ) |>
    dplyr::mutate(
      # see utils.R
      aggregation_depth =
        aggregation_depth(
          observed_unit
        )
    )

  # ------------------------------------------------------------
  # Normalize observed units
  # ------------------------------------------------------------

  provenance <- provenance |>
    dplyr::mutate(
      observed_unit =
        normalize_context_roots(observed_unit)
    )

  # ------------------------------------------------------------
  # Scale-aware contextual matching
  # ------------------------------------------------------------

  out <- provenance |>
    dplyr::left_join(
      roots_tbl,
      by = c(
        "observed_unit",
        "aggregation_depth"
      )
    ) |>
    dplyr::mutate(
      included =
        !is.na(included)
    )

  # ------------------------------------------------------------
  # Return
  # ------------------------------------------------------------

  out |>
    dplyr::relocate(
      included,
      .after = observed_unit
    ) |>
    dplyr::arrange(
      dplyr::desc(included),
      aggregation_depth,
      observed_unit
    )
}
