#' Derive operational reuse metrics from contextual resources
#'
#' @description
#' Derives lightweight operational reuse and reconstruction metrics
#' from contextual resource observations.
#'
#' The function summarizes how frequently contextual resources appear:
#'
#' - across observations;
#' - across Record Sets;
#' - across storage locations;
#' - and across time.
#'
#' The resulting metrics support:
#'
#' - duplication analysis;
#' - reconstruction workflows;
#' - synchronized workspace inspection;
#' - cross-project reuse detection;
#' - provenance-aware reporting;
#' - and forensic review workflows.
#'
#' In many filesystem workflows, the resulting metrics approximate
#' how digital resources ("files") evolve, move, synchronize, and
#' reappear across operational environments.
#'
#' The function is designed to work together with:
#'
#' - [read_snapshot()]
#' - [snapshot_to_reconstruction_context()]
#' - [create_record_set()]
#'
#' as part of layered provenance-aware reconstruction workflows.
#'
#' @param x A `data.frame` or tibble containing contextual resource
#'   observations.
#'
#' @param resource_id Character scalar identifying the column
#'   representing contextual resource identity.
#'
#' Defaults to `"resource_id"`.
#'
#' @param record_set_id Character scalar identifying the contextual
#'   Record Set membership column.
#'
#' Defaults to `"record_set_id"`.
#'
#' @param storage_path_id Character scalar identifying the
#'   storage-scoped path identifier column.
#'
#' Defaults to `"storage_path_id"`.
#'
#' @param timestamp Character scalar identifying the timestamp column
#'   used for temporal reconstruction.
#'
#' Defaults to `"mtime"`.
#'
#' @param location Character scalar identifying the human-readable
#'   location column.
#'
#' Defaults to `"full_path"`.
#'
#' @return
#' A tibble containing operational reuse metrics.
#'
#' Typical output variables include:
#'
#' - `n_observations`
#' - `n_record_sets`
#' - `n_paths`
#' - `first_seen`
#' - `last_seen`
#' - `locations`
#'
#' @details
#' The function intentionally derives lightweight operational metrics
#' only.
#'
#' It does not:
#'
#' - infer authoritative identity;
#' - reconcile evolving resources;
#' - perform provenance reasoning;
#' - determine archival significance;
#' - replace curatorial interpretation.
#'
#' Metrics are derived from contextual operational observations and
#' should be interpreted as analytical indicators rather than
#' authoritative documentary assertions.
#'
#' @examples
#' toy_resources <- tibble::tibble(
#'   resource_id = c(
#'     "res_001",
#'     "res_001",
#'     "res_002"
#'   ),
#'   record_set_id = c(
#'     "project_a",
#'     "project_b",
#'     "project_a"
#'   ),
#'   storage_path_id = c(
#'     "laptop::analysis.R",
#'     "backup::analysis.R",
#'     "laptop::report.qmd"
#'   ),
#'   mtime = as.POSIXct(c(
#'     "2025-01-01",
#'     "2025-01-03",
#'     "2025-01-02"
#'   )),
#'   full_path = c(
#'     "D:/project/analysis.R",
#'     "E:/backup/analysis.R",
#'     "D:/project/report.qmd"
#'   )
#' )
#'
#' derive_reuse_metrics(
#'   toy_resources
#' )
#'
#' @importFrom dplyr group_by summarise n n_distinct
#' @importFrom rlang .data
#' @export

derive_reuse_metrics <- function(
  x,
  resource_id = "resource_id",
  record_set_id = "record_set_id",
  storage_path_id = "storage_path_id",
  timestamp = "mtime",
  location = "full_path"
) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame or tibble",
      call. = FALSE
    )
  }

  required_cols <- c(
    resource_id,
    record_set_id,
    storage_path_id,
    timestamp,
    location
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  x |>
    dplyr::group_by(
      .data[[resource_id]]
    ) |>
    dplyr::summarise(
      n_observations =
        dplyr::n(),
      n_record_sets =
        dplyr::n_distinct(
          .data[[record_set_id]]
        ),
      n_paths =
        dplyr::n_distinct(
          .data[[storage_path_id]]
        ),
      first_seen =
        min(
          .data[[timestamp]],
          na.rm = TRUE
        ),
      last_seen =
        max(
          .data[[timestamp]],
          na.rm = TRUE
        ),
      locations =
        paste(
          unique(.data[[location]]),
          collapse = " | "
        ),
      .groups = "drop"
    )
}
