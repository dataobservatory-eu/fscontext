#' Construct recursive contextual structural paths
#'
#' Construct normalized context-relative structural paths from
#' filesystem snapshot observations.
#'
#' @details
#' The function creates an intermediate structural abstraction
#' layer between:
#'
#' - observational filesystem paths;
#' - contextual semantic interpretation.
#'
#' Observations are:
#'
#' - matched to contextual roots;
#' - normalized into context-relative structural paths;
#' - recursively expanded into hierarchical structural components.
#'
#' This enables:
#'
#' - contextual filesystem diagnostics;
#' - recursive structural matching;
#' - contextual coverage analysis;
#' - semantic refinement workflows.
#'
#' For example:
#'
#' \describe{
#'
#'   \item{
#'   `"R/import/helpers.R"`
#'   }{
#'   expands into:
#'
#'   - `"R"`
#'   - `"R/import"`
#'   }
#'
#'   \item{
#'   `"tests/testthat/test-import.R"`
#'   }{
#'   expands into:
#'
#'   - `"tests"`
#'   - `"tests/testthat"`
#'   }
#' }
#'
#' The function explicitly separates:
#'
#' \describe{
#'
#'   \item{`rel_path`}{
#'   Storage-relative observational filesystem path.
#'   }
#'
#'   \item{`structural_path`}{
#'   Context-relative normalized structural path.
#'   }
#'
#'   \item{`explored_path`}{
#'   Recursively expanded structural abstraction layer.
#'   }
#' }
#'
#' @param snapshot A filesystem snapshot tibble containing at
#'   least:
#'
#'   - `full_path`
#'   - `rel_path`
#'
#' @param contexts A contextual reconstruction object
#'   containing contextual roots.
#'
#' @return
#' A tibble containing recursively expanded contextual
#' structural paths.
#'
#' The returned tibble includes:
#'
#' \describe{
#'
#'   \item{context}{
#'   Contextual ecosystem identifier.
#'   }
#'
#'   \item{root}{
#'   Matched contextual root.
#'   }
#'
#'   \item{rel_path}{
#'   Original storage-relative observational path.
#'   }
#'
#'   \item{structural_path}{
#'   Context-relative normalized structural path.
#'   }
#'
#'   \item{explored_path}{
#'   Recursively expanded structural abstraction.
#'   }
#' }
#'
#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' mini_context <- list(
#'   alpha = "D:/_packages/fscontextdemo"
#' )
#'
#' mini_snapshot <- fscontextdemo_snapshot_02[
#'   c(1, 3, 5, 10),
#' ]
#'
#' structural_paths <- construct_structural_paths(
#'   snapshot = mini_snapshot,
#'   contexts = mini_context
#' )
#'
#' structural_paths[, c("context", "structural_path", "explored_path")]
#'
#' @importFrom dplyr mutate filter cross_join if_else
#' @importFrom tidyr unnest
#' @importFrom purrr imap_dfr map map2_chr map_chr
#' @importFrom tibble tibble
#'
#' @export
construct_structural_paths <- function(
  snapshot,
  contexts
) {
  # ------------------------------------------------------------
  # Validate inputs
  # ------------------------------------------------------------

  if (!is.data.frame(snapshot)) {
    stop(
      "`snapshot` must be a data.frame.",
      call. = FALSE
    )
  }

  if (!is.list(contexts)) {
    stop(
      "`contexts` must be a list.",
      call. = FALSE
    )
  }
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
  # Flatten contextual roots
  # ------------------------------------------------------------

  roots_tbl <- purrr::imap_dfr(
    contexts,
    function(roots, context_name) {
      tibble::tibble(
        context = context_name,
        root =
          normalize_context_roots(
            roots
          )
      )
    }
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
  # Match snapshot rows to contextual roots
  # ------------------------------------------------------------

  matched_snapshot <- snapshot |>
    dplyr::cross_join(
      roots_tbl
    ) |>
    dplyr::filter(
      full_path == root |

        stringr::str_starts(
          full_path,
          paste0(root, "/")
        )
    )

  # ------------------------------------------------------------
  # Remove contextual root stem
  # ------------------------------------------------------------

  matched_snapshot <- matched_snapshot |>
    dplyr::mutate(
      structural_path =

        purrr::map2_chr(
          root,
          rel_path,
          function(root_path, relative_path) {
            # normalize separators
            relative_path <- gsub(
              "\\\\",
              "/",
              relative_path
            )

            # derive storage-relative root stem
            root_stem <- sub(
              "^[A-Z]:/",
              "",
              normalize_context_roots(root_path)
            )

            # remove BOTH possible stems
            relative_path <- sub(
              paste0("^", root_stem, "/"),
              "",
              relative_path
            )

            root_name <- basename(root_path)

            relative_path <- sub(
              paste0("^", root_name, "/"),
              "",
              relative_path
            )

            relative_path
          }
        )
    )

  # ------------------------------------------------------------
  # Expand recursive structural paths
  # ------------------------------------------------------------

  matched_snapshot |>
    dplyr::mutate(
      dir_path =
        dirname(structural_path),
      dir_path =
        dplyr::if_else(
          dir_path == ".",
          NA_character_,
          dir_path
        ),
      explored_path =
        purrr::map(
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
}
