#' Add contextual identifiers to snapshot observations
#'
#' Enriches filesystem observations with deterministic contextual
#' identifiers used for longitudinal, cross-storage, and forensic
#' reconstruction workflows.
#'
#' @details
#' The function preserves the original observational rows and adds
#' contextual identifiers derived from:
#'
#' - storage context;
#' - filesystem location;
#' - observation time.
#'
#' The package deliberately separates filesystem observation from
#' later analytical interpretation and documentary aggregation.
#'
#' The function therefore:
#'
#' - enriches filesystem observations with contextual identifiers;
#' - supports repeated observation tracking across time;
#' - supports comparison across storage systems;
#' - does not construct Record Sets or higher-level documentary
#'   aggregations.
#'
#' The added identifiers support:
#'
#' - reconstruction of distributed work environments;
#' - provenance-aware analytical workflows;
#' - longitudinal filesystem analysis;
#' - cross-storage comparison of observations.
#'
#' Added variables:
#'
#' - `storage_full_path`:
#'   globally contextualised filesystem locator
#'   (`storage_id::full_path`);
#'
#' - `storage_path_id`:
#'   deterministic storage-scoped filesystem identifier
#'   (`storage_id::rel_path`);
#'
#' - `observation_id`:
#'   deterministic identifier of a specific filesystem observation,
#'   combining storage context, relative path, and observation time.
#'
#' @param df A snapshot `data.frame` created by [scan_storage()]
#'   or [read_snapshot()].
#'
#' @return
#' A `data.frame` enriched with additional contextual identifier
#' variables.
#'
#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' snapshots <- add_snapshot_context(fscontextdemo_snapshot_02)
#'
#' head(
#'   snapshots[
#'     ,
#'     c(
#'       "storage_full_path",
#'       "storage_path_id",
#'       "observation_id"
#'     )
#'   ]
#' )
#' @importFrom dplyr mutate
#' @export

add_snapshot_context <- function(df) {
  # ------------------------------------------------------------
  # Backward-compatible storage-scoped file identity
  # ------------------------------------------------------------

  if (!"storage_path_id" %in% names(df)) {
    df$storage_path_id <- paste(
      df$storage_id,
      df$rel_path,
      sep = "::"
    )
  }

  # ------------------------------------------------------------
  # Contextual enrichment
  # ------------------------------------------------------------

  df |>
    dplyr::mutate(
      # --- globally contextualised filesystem locator ---
      # helps distinguish similar local paths across storages
      # and observation environments
      storage_full_path =
        paste(storage_id, full_path, sep = "::"),

      # --- observation event identity ---
      # uniquely identifies one observed filesystem
      # Instantiation at a specific observation time

      observation_id =
        paste(
          storage_id,
          rel_path,
          format(scan_time, "%Y%m%d-%H%M%S"),
          sep = "::"
        )
    )
}
