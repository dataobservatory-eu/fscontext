#' Iteratively refine semantic assertions using a rulebook
#'
#' Apply a compiled semantic rulebook sequentially to a target
#' semantic assertion vector.
#'
#' @details
#' `refine_by_rulebook()` wraps an iterative refinement workflow
#' where each semantic refinement stage operates on the semantic
#' state produced by the previous stage.
#'
#' The function is designed for operational semantic stabilization
#' workflows where semantic assertions progressively mature through
#' deterministic contextual refinement.
#'
#' Each refinement stage creates a new refinement column,
#' preserving semantic progression for inspection, auditing, and
#' review.
#'
#' Rulebooks are typically created with
#' [compile_rulebook()] and operate on semantic operationalisations
#' produced with:
#'
#' - [prelabel()];
#' - [as.character()];
#' - [as_character()];
#' - [refine()].
#'
#' Conceptually, the function behaves similarly to an iterative
#' `purrr::reduce()` workflow applied to semantic refinement
#' stages.
#'
#' @param x A data frame.
#'
#' @param target Bare or quoted name of the initial semantic
#' assertion column.
#'
#' @param rulebook A compiled rulebook object created with
#' `compile_rulebook()`.
#'
#' @param prefix Prefix used for generated refinement columns.
#' Defaults to the target column name.
#'
#' @param keep_intermediate Logical.
#' If `TRUE`, keeps all intermediate refinement columns.
#' If `FALSE`, returns only the final refinement column.
#'
#' @param final_name Name of the final refinement column.
#'
#' @return
#' A tibble with progressively refined semantic assertions.
#'
#' @export
#'
#' @seealso [refine()], [compile_rulebook()]
#' @importFrom dplyr mutate select all_of
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom rlang enquo as_name .data :=
refine_by_rulebook <- function(
  x,
  target,
  rulebook,
  prefix = NULL,
  keep_intermediate = TRUE,
  final_name = "final"
) {
  ## Validation ------------------------------------------------

  stopifnot(is.data.frame(x))

  if (!is.list(rulebook)) {
    stop("`rulebook` must be a compiled rulebook list.")
  }

  ## Tidy evaluation -------------------------------------------

  target_quo <- rlang::enquo(target)

  target_name <- rlang::as_name(target_quo)

  if (!target_name %in% names(x)) {
    stop(
      "Target column not found: ",
      target_name
    )
  }

  if (is.null(prefix)) {
    prefix <- target_name
  }

  ## Progress bar ----------------------------------------------

  pb <- txtProgressBar(
    min = 0,
    max = length(rulebook),
    style = 3
  )

  ## Sequential refinement -------------------------------------

  refined_data <-
    purrr::reduce(
      seq_along(rulebook),
      .init = x,
      .f = function(data, i) {
        setTxtProgressBar(pb, i)

        previous_col <-
          if (i == 1) {
            target_name
          } else {
            paste0(prefix, "_", i - 1, "_ref")
          }

        new_col <-
          paste0(prefix, "_", i, "_ref")

        current_rule <-
          rulebook[[i]]

        data %>%
          dplyr::mutate(
            !!new_col :=
              refine(
                x = .,
                target = .data[[previous_col]],
                rules =
                  current_rule$rules,
                by =
                  current_rule$by,
                match =
                  current_rule$match,
                assertion =
                  current_rule$assertion
              )
          )
      }
    )

  close(pb)

  ## Final naming ----------------------------------------------

  final_col <-
    paste0(
      prefix,
      "_",
      length(rulebook),
      "_ref"
    )

  refined_data <-
    refined_data %>%
    dplyr::mutate(
      !!paste0(prefix, "_", final_name) :=
        .data[[final_col]]
    )

  ## Optional cleanup ------------------------------------------

  if (!keep_intermediate) {
    keep_cols <- c(
      names(x),
      paste0(prefix, "_", final_name)
    )

    refined_data <-
      refined_data %>%
      dplyr::select(
        dplyr::all_of(keep_cols)
      )
  }

  refined_data
}
