#' Compile a semantic refinement rulebook
#'
#' Compile a tidy semantic refinement rulebook into grouped
#' refinement specifications suitable for
#' [refine_by_rulebook()] workflows.
#'
#' `compile_rulebook()` transforms a long-form rule table into a
#' lightweight operational structure for iterative semantic
#' stabilization.
#'
#' Each compiled refinement stage contains:
#'
#' - matching variables;
#' - matching semantics;
#' - observational matching patterns;
#' - refined semantic assertions.
#'
#' Rulebooks support workflows where semantic interpretations are
#' progressively stabilized through deterministic contextual
#' matching rather than fully predefined ontology structures.
#'
#' @param rulebook A data frame or tibble containing semantic
#' refinement rules.
#'
#' The input must contain:
#'
#' - `"refine_id"`: refinement stage identifier;
#' - `"variable"`: matching variable;
#' - `"match"`: matching semantics;
#' - `"pattern"`: matching pattern;
#' - `"refined_assertion"`: refined semantic assertion.
#'
#' @return
#' A list of compiled semantic refinement specifications suitable
#' for [refine_by_rulebook()].
#'
#' Each list element contains:
#'
#' - `"refine_id"`: refinement stage identifier;
#' - `"rules"`: wide matching rule table;
#' - `"by"`: matching variables;
#' - `"match"`: matching semantics;
#' - `"assertion"`: refined semantic assertion.
#'
#' @details
#' The function intentionally preserves tidy relational semantics
#' in the rulebook representation while creating a lightweight
#' operational structure for iterative semantic refinement.
#'
#' Rulebooks are designed to support workflows where semantic
#' stabilization emerges gradually through deterministic matching
#' operations rather than through fully predefined ontological
#' structures.
#'
#' @examples
#'
#' rulebook <- data.frame(
#'   refine_id = c(
#'     "refine_1",
#'     "refine_1",
#'     "refine_2"
#'   ),
#'   variable = c(
#'     "extension",
#'     "filename",
#'     "extension"
#'   ),
#'   match = c(
#'     "exact",
#'     "starts_with",
#'     "exact"
#'   ),
#'   pattern = c(
#'     "png",
#'     "film",
#'     "csv"
#'   ),
#'   refined_assertion = c(
#'     "visualisation",
#'     "visualisation",
#'     "tabular_data"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' compile_rulebook(rulebook)
#'
#' @seealso [refine()],  [refine_by_rulebook()]
#' @importFrom dplyr group_by group_split select
#' @importFrom purrr map
#' @importFrom tidyr pivot_wider
#'
#' @export
compile_rulebook <- function(rulebook) {
  required_cols <- c(
    "refine_id",
    "variable",
    "match",
    "pattern",
    "refined_assertion"
  )

  missing_cols <-
    setdiff(
      required_cols,
      names(rulebook)
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

  compiled <-
    rulebook |>
    dplyr::group_by(
      refine_id
    ) |>
    dplyr::group_split()

  purrr::map(
    compiled,
    function(tbl) {
      rules <-
        tbl |>
        dplyr::select(
          variable,
          pattern
        ) |>
        tidyr::pivot_wider(
          names_from = variable,
          values_from = pattern
        )

      list(
        refine_id =
          unique(tbl$refine_id),
        rules =
          rules,
        by =
          tbl$variable,
        match =
          tbl$match,
        assertion =
          unique(tbl$refined_assertion)
      )
    }
  )
}
