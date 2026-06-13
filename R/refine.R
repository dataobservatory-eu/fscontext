#' Refine semantic assertions through contextual matching
#'
#' `refine()` incrementally stabilizes semantic assertions through
#' deterministic contextual matching rules while preserving row
#' cardinality and the original observational universe.
#'
#' The function is designed for lightweight semantic refinement
#' workflows where semantic interpretations mature gradually
#' through ordinary tidyverse operations.
#'
#' Matching observations are identified through configurable
#' matching semantics applied to one or more observational
#' variables.
#'
#' Supported matching semantics include:
#'
#' - `"exact"` relational equality;
#' - `"starts_with"` hierarchical prefix matching;
#' - `"ends_with"` suffix matching;
#' - `"contains"` substring detection.
#'
#' Matching positions in the target vector are replaced by
#' refined semantic assertions.
#'
#' Unmatched values remain unchanged.
#'
#' `refine()` intentionally never:
#'
#' - removes rows;
#' - reshapes tables;
#' - modifies unrelated observations.
#'
#' This makes refinement stages auditable, reversible, and
#' compatible with iterative semantic stabilization workflows.
#'
#' @details
#' `refine()` operates on semantic operationalisations produced
#' through workflows such as:
#'
#' - [prelabel()];
#' - [as.character()];
#' - [as_character()];
#' - previous refinement stages.
#'
#' Rather than enforcing formally complete ontology semantics,
#' the function provides a lightweight operational mechanism for
#' progressively stabilizing semantic interpretations inside
#' ordinary analytical workflows.
#'
#' Multiple refinement stages may later mature into:
#'
#' - controlled vocabularies;
#' - [labelled::labelled()] vectors;
#' - [dataset::defined()] vectors;
#' - semantically enriched datasets
#' - compatible with iterative semantic workflows.
#' @details
#' `refine()` operates on semantic operationalisations produced
#' through workflows such as:
#'
#' - [prelabel()];
#' - [as.character()];
#' - [as_character()];
#' - earlier refinement stages.
#'
#' The function does not attempt to construct formally complete
#' semantic graphs or enforce ontology-level consistency.
#'
#' Instead, it provides a lightweight operational mechanism for
#' progressively stabilizing semantic interpretations inside
#' ordinary tidyverse workflows.
#'
#' This approach is particularly useful when working with:
#'
#' - partially harmonised datasets;
#' - inconsistent coding systems;
#' - ambiguous metadata;
#' - hierarchical filesystem structures;
#' - exploratory semantic reconstruction workflows.
#'
#' Multiple refinement stages may later mature into:
#'
#' - controlled vocabularies;
#' - formally defined semantic vectors;
#' - semantically enriched datasets;
#' - or graph-based semantic representations.
#' @examples
#'
#' files <- tibble::tibble(
#'   filename = c(
#'     "filmA.png",
#'     "filmB.png",
#'     "film.xlsx",
#'     "fill.png"
#'   ),
#'   extension = c(
#'     "png",
#'     "png",
#'     "xlsx",
#'     "png"
#'   )
#' )
#'
#' out <- refine(
#'   x = files,
#'   target =
#'     rep(
#'       "unresolved",
#'       nrow(files)
#'     ),
#'   rules =
#'     tibble::tibble(
#'       filename = "film",
#'       extension = "png"
#'     ),
#'   by = c(
#'     "filename",
#'     "extension"
#'   ),
#'   match = c(
#'     "starts_with",
#'     "exact"
#'   ),
#'   assertion =
#'     "film_visualisation"
#' )
#'
#' out
#'
#' @param x A data frame or tibble.
#' @param target Name of the target column to refine.
#' @param rules A rule table or compiled rulebook.
#' @param by Optional grouping variables used during refinement.
#' @param assertion Optional assertion text recorded in provenance.
#' @param comment Optional comment attached to the refinement step.
#' @param match Matching strategy. Defaults to `"first"`.
#'
#' @seealso [refine_by_rulebook()], [compile_rulebook()]
#' @importFrom dplyr across all_of filter mutate row_number semi_join
#' @importFrom purrr map_lgl
#' @importFrom stringr fixed str_detect str_ends str_starts
#'
#' @export
refine <- function(
  x,
  target = NULL,
  rules,
  by,
  assertion,
  comment = NULL,
  match = "exact"
) {
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(rules))

  allowed_matches <- c(
    "exact",
    "starts_with",
    "ends_with",
    "contains"
  )

  if (!all(by %in% names(x))) {
    stop(
      "All `by` columns must exist in `x`.",
      call. = FALSE
    )
  }

  if (!all(by %in% names(rules))) {
    stop(
      "All `by` columns must exist in `rules`.",
      call. = FALSE
    )
  }

  if (length(match) == 1) {
    match <-
      rep(
        match,
        length(by)
      )
  }

  if (length(match) != length(by)) {
    stop(
      "`match` must have length 1 or length(by).",
      call. = FALSE
    )
  }

  if (!all(match %in% allowed_matches)) {
    stop(
      "Invalid matching semantics in `match`.",
      call. = FALSE
    )
  }

  names(match) <- by

  if (is.null(target)) {
    target <-
      rep(
        NA_character_,
        nrow(x)
      )
  }

  if (length(target) != nrow(x)) {
    stop(
      "`target` must have the same length as `nrow(x)`.",
      call. = FALSE
    )
  }

  working_tbl <-
    x |>
    dplyr::mutate(
      .row_id =
        dplyr::row_number()
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(by),
        \(z)
        as.character(
          unclass(z)
        )
      )
    )

  rules_tbl <-
    rules |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(by),
        \(z)
        as.character(
          unclass(z)
        )
      )
    )

  exact_only <-
    all(match == "exact")

  if (exact_only) {
    matched <-
      working_tbl |>
      dplyr::semi_join(
        rules_tbl,
        by = by
      )
  } else {
    matched_rows <-
      purrr::map_lgl(
        seq_len(nrow(working_tbl)),
        function(i) {
          any(
            purrr::map_lgl(
              seq_len(nrow(rules_tbl)),
              function(j) {
                all(
                  purrr::map_lgl(
                    by,
                    function(col) {
                      current_match <-
                        match[[col]]

                      x_value <-
                        working_tbl[[col]][i]

                      rule_value <-
                        rules_tbl[[col]][j]

                      if (
                        is.na(x_value) ||
                          is.na(rule_value)
                      ) {
                        return(FALSE)
                      }

                      if (current_match == "exact") {
                        return(
                          identical(
                            x_value,
                            rule_value
                          )
                        )
                      }

                      if (
                        current_match ==
                          "starts_with"
                      ) {
                        return(
                          stringr::str_starts(
                            x_value,
                            rule_value
                          )
                        )
                      }

                      if (
                        current_match ==
                          "ends_with"
                      ) {
                        return(
                          x_value ==
                            rule_value ||

                            stringr::str_ends(
                              x_value,
                              rule_value
                            )
                        )
                      }

                      if (
                        current_match ==
                          "contains"
                      ) {
                        return(
                          stringr::str_detect(
                            x_value,
                            stringr::fixed(
                              rule_value
                            )
                          )
                        )
                      }

                      FALSE
                    }
                  )
                )
              }
            )
          )
        }
      )

    matched <-
      working_tbl |>
      dplyr::filter(
        matched_rows
      )
  }

  out <- target

  out[matched$.row_id] <-
    rep_len(
      assertion,
      length(matched$.row_id)
    )

  attr(
    out,
    "comment"
  ) <- comment

  out
}
