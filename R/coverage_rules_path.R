#' Evaluate contextual rule coverage for structural paths
#'
#' Evaluates how filesystem observations align with a contextual
#' path-rule model.
#'
#' @description
#' coverage_rules_path() expands observed filesystem paths into
#' recursive structural components and evaluates whether those
#' components are covered by contextual path rules.
#'
#' The function provides a diagnostic view of contextual coverage.
#' It identifies which observed filesystem structures are recognised
#' by a rulebook and which remain unmatched.
#'
#' This makes it useful for:
#'
#' - contextual reconstruction;
#' - semantic stabilisation workflows;
#' - rulebook development and refinement;
#' - coverage diagnostics;
#' - filesystem archaeology;
#' - exploratory analysis of operational structures.
#'
#' @details
#' Structural matching is recursive.
#'
#' For example:
#'
#' - "R/import/helpers.R" expands to:
#' - "R"
#' - "R/import"
#'
#' - "tests/testthat/test-import.R" expands to:
#' - "tests"
#' - "tests/testthat"
#'
#' Expanded structural paths are evaluated against contextual
#' path rules associated with the matching contextual root.
#'
#' The function operates on filesystem observations and explicit
#' contextual rules. It does not infer:
#'
#' - Activities;
#' - Events;
#' - Record Sets;
#' - provenance relationships.
#'
#' Instead, it evaluates whether observed filesystem structures
#' are covered by an existing contextual model.
#'
#' Unmatched structures may indicate:
#'
#' - missing contextual rules;
#' - evolving workflows;
#' - operational noise;
#' - areas requiring further semantic stabilisation.
#'
#' @param snapshot A filesystem snapshot data.frame
#' containing at least:
#'
#' - full_path;
#' - rel_path.
#'
#' @param contexts A contextual reconstruction object containing
#' contextual roots and path rules.
#'
#' @return
#' A tibble describing structural path-rule coverage.
#'
#' The returned tibble includes:
#'
#' \describe{
#'
#' \item{context}{
#' Contextual environment associated with the matched rule.
#' }
#'
#' \item{root}{
#' Contextual root used for evaluation.
#' }
#'
#' \item{explored_path}{
#' Recursively expanded structural path.
#' }
#'
#' \item{matched_rule}{
#' Path rule evaluated against the structural path.
#' }
#'
#' \item{activity}{
#' Rule outcome associated with the matched rule.
#' }
#'
#' \item{matched}{
#' Logical indicator showing whether the structural path is
#' covered by the rule.
#' }
#' }
#'
#' @examples
#' small_snapshot <- tibble::tibble(
#'   full_path = c(
#'     "D:/packages/fscontext/R/import/helpers.R",
#'     "D:/packages/fscontext/tests/testthat/test-import.R",
#'     "D:/packages/fscontext/data-raw/input.csv"
#'   ),
#'   rel_path = c(
#'     "R/import/helpers.R",
#'     "tests/testthat/test-import.R",
#'     "data-raw/input.csv"
#'   )
#' )
#'
#' small_context <- list(
#'   contexts = list(
#'     fscontext = list(
#'       roots = "D:/packages/fscontext",
#'       rules = list(
#'         path = c(
#'           "R" = "software_development",
#'           "tests/testthat" = "unit_testing",
#'           "data-raw" = "etl"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' coverage_rules_path(
#'   snapshot = small_snapshot,
#'   contexts = small_context
#' )
#'
#' @seealso
#' [construct_structural_paths()],
#' [refine_by_rulebook()],
#' [compile_rulebook()]
#'
#' @importFrom dplyr mutate filter cross_join
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom purrr imap_dfr map map_chr
#'
#' @export

coverage_rules_path <- function(
  snapshot,
  contexts
) {
  # ------------------------------------------------------------
  # Validate inputs
  # ------------------------------------------------------------

  stopifnot(
    is.data.frame(snapshot),
    is.list(contexts)
  )

  required_cols <- c(
    "full_path",
    "rel_path"
  )

  missing_cols <- setdiff(
    required_cols,
    names(snapshot)
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
  # Flatten path rules
  # ------------------------------------------------------------

  path_rules <- purrr::imap_dfr(
    contexts$contexts,
    function(ctx, context_name) {
      if (
        is.null(ctx$rules$path)
      ) {
        return(NULL)
      }

      tibble::tibble(
        context =
          context_name,
        root =
          rep(
            ctx$roots,
            each =
              length(
                names(ctx$rules$path)
              )
          ),
        matched_rule =
          rep(
            names(ctx$rules$path),
            times =
              length(ctx$roots)
          ),
        activity =
          rep(
            unname(ctx$rules$path),
            times =
              length(ctx$roots)
          )
      )
    }
  )

  # ------------------------------------------------------------
  # Normalize roots
  # ------------------------------------------------------------

  path_rules <- path_rules |>
    dplyr::mutate(
      root =
        normalize_context_roots(root)
    )

  # ------------------------------------------------------------
  # Normalize snapshot paths
  # ------------------------------------------------------------

  snapshot <- snapshot |>
    dplyr::mutate(
      full_path =
        normalize_context_roots(full_path),
      rel_path =
        gsub(
          "\\\\",
          "/",
          rel_path
        )
    )

  # ------------------------------------------------------------
  # Expand recursive structural paths
  # ------------------------------------------------------------

  expanded_paths <- snapshot |>
    dplyr::mutate(
      dir_path = dirname(rel_path),
      dir_path = dplyr::if_else(
        dir_path == ".",
        NA_character_,
        dir_path
      ),
      explored_path = purrr::map(
        dir_path,
        function(path) {
          if (is.na(path)) {
            return(character(0))
          }

          parts <- strsplit(
            path,
            "/",
            fixed = TRUE
          )[[1]]

          purrr::map_chr(
            seq_along(parts),
            function(i) {
              paste(
                parts[
                  seq_len(i)
                ],
                collapse = "/"
              )
            }
          )
        }
      )
    ) |>
    tidyr::unnest(
      explored_path
    )

  # ------------------------------------------------------------
  # Match contextual rules
  # ------------------------------------------------------------

  out <- expanded_paths |>
    dplyr::cross_join(
      path_rules
    ) |>
    dplyr::filter(
      full_path == root |
        stringr::str_starts(
          full_path,
          paste0(root, "/")
        )
    ) |>
    dplyr::mutate(
      matched =
        explored_path == matched_rule |
          stringr::str_starts(
            explored_path,
            paste0(
              matched_rule,
              "/"
            )
          )
    )

  out
}
