#' Reconstruct a contextual observational Record Set from filesystem snapshots
#'
#' @description
#' Reconstructs a contextual observational corpus from one or more
#' filesystem snapshot fragments.
#'
#' The function:
#'
#' \enumerate{
#'   \item merges observational filesystem snapshots;
#'   \item filters observations to selected contextual roots;
#'   \item enriches observations with contextual identifiers;
#'   \item derives lightweight structural grouping heuristics;
#'   \item creates a contextual Record Set projection.
#' }
#'
#' The workflow is optimized for:
#'
#' - forensic reconstruction;
#' - filesystem archaeology;
#' - exploratory analytical workflows;
#' - development environment reconstruction;
#' - operational reporting.
#'
#' Unlike [snapshot_to_recordset_df()], this function intentionally
#' prioritizes analytical reconstruction over preservation-oriented
#' semantic assertions.
#'
#' @param snapshot_files Character vector of `.rds` snapshot files
#'   created with [snapshot_storage()].
#'
#' @param roots Character vector of contextual root paths used
#'   for observational selection.
#'
#' @param exclude_patterns Character vector of exclusion patterns
#'   passed to [subset_snapshot()].
#'
#' @return
#' A contextual observational reconstruction table enriched with:
#'
#' - contextual observational identifiers;
#' - storage-aware path identifiers;
#' - structural grouping heuristics;
#' - lightweight contextual Record Set projections.
#'
#' Core observational variables typically include:
#'
#' - `storage_id`;
#' - `person_id`;
#' - `full_path`;
#' - `rel_path`;
#' - `filename`;
#' - `extension`;
#' - `mtime`;
#' - `scan_time`.
#'
#' Contextual enrichment variables may include:
#'
#' - `inst_id`;
#' - `storage_path_id`;
#' - `observation_id`;
#' - `structural_group`;
#' - `component`;
#' - `record_set_id`;
#' - `resource_id`;
#' - `locator_path`.
#'
#' @details
#' Snapshot fragments are merged observationally.
#'
#' Duplicate filesystem observations are intentionally preserved
#' because the same resource may legitimately appear across:
#'
#' - multiple machines;
#' - multiple storage contexts;
#' - repeated scans;
#' - synchronised working environments.
#'
#' The resulting object remains observational and analytical.
#'
#' Structural grouping heuristics are lightweight filesystem-derived
#' operational projections and do not imply authoritative archival
#' Record Set construction.
#'
#' The function serves as the foundational reconstruction layer for:
#'
#' - analytical enrichment workflows;
#' - reconstruction reporting;
#' - semantic preservation wrappers such as
#'   [snapshot_to_recordset_df()].
#'
#' @seealso
#' [snapshot_to_recordset_df()],
#' [subset_snapshot()],
#' [add_snapshot_context()],
#' [add_structural_groups()].
#'
#' @examples
#' data("fscontextdemo_snapshot_01")
#'
#' tmp <- tempfile(fileext = ".rds")
#' saveRDS(fscontextdemo_snapshot_01, tmp)
#'
#' snapshot_to_reconstruction_context(
#'   snapshot_files = tmp,
#'   roots = "D:/_packages/fscontextdemo/R"
#' )
#' @importFrom purrr map_dfr
#' @importFrom dplyr arrange
#' @export

snapshot_to_reconstruction_context <- function(
  snapshot_files,
  roots,
  exclude_patterns = "\\.Rcheck"
) {
  # ------------------------------------------------------------------
  # Validate snapshot files
  # ------------------------------------------------------------------

  if (!is.character(snapshot_files)) {
    stop(
      "`snapshot_files` must be a character vector of .rds snapshot paths.",
      call. = FALSE
    )
  }

  if (length(snapshot_files) == 0) {
    stop(
      "`snapshot_files` must contain at least one snapshot file.",
      call. = FALSE
    )
  }

  missing_snapshot_files <- snapshot_files[
    !file.exists(snapshot_files)
  ]

  if (length(missing_snapshot_files) > 0) {
    stop(
      paste0(
        "The following snapshot files do not exist:\n",
        paste(missing_snapshot_files, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Validate contextual roots
  # ------------------------------------------------------------------

  if (!is.character(roots)) {
    stop(
      "`roots` must be a character vector of filesystem root paths.",
      call. = FALSE
    )
  }

  if (length(roots) == 0) {
    stop(
      "`roots` must contain at least one contextual root path.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # LOAD OBSERVATIONAL SNAPSHOTS
  # ------------------------------------------------------------------
  #
  # Snapshot fragments are merged observationally.
  #
  # Duplicate filesystem observations are intentionally preserved,
  # because the same resource may legitimately appear across:
  #
  # - multiple machines;
  # - multiple storage contexts;
  # - repeated scans;
  # - synchronised working environments.
  # ------------------------------------------------------------------

  merged_snapshots <- purrr::map_dfr(
    snapshot_files,
    readRDS
  )

  merged_snapshots <- merged_snapshots |>
    dplyr::arrange(
      storage_id,
      full_path,
      scan_time
    )

  # ------------------------------------------------------------------
  # CONTEXTUAL SELECTION
  # ------------------------------------------------------------------
  #
  # Select observational filesystem Instantiations associated with
  # the supplied contextual roots.
  #
  # Selection remains observational and does not imply authoritative
  # documentary aggregation.
  # ------------------------------------------------------------------

  tmp_snapshot <- tempfile(fileext = ".rds")

  saveRDS(
    merged_snapshots,
    tmp_snapshot
  )

  contextual_snapshot <- subset_snapshot(
    snapshot_path = tmp_snapshot,
    folder_path = roots,
    exclude_patterns = exclude_patterns
  )

  # ------------------------------------------------------------------
  # Empty contextual selection
  # ------------------------------------------------------------------

  if (nrow(contextual_snapshot) == 0) {
    stop(
      paste0(
        "No filesystem observations matched the supplied contextual roots.\n\n",
        "Checked roots:\n",
        paste(roots, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # CONTEXTUAL ENRICHMENT
  # ------------------------------------------------------------------
  #
  # Add:
  #
  # - contextual observational identifiers;
  # - storage-aware path identifiers;
  # - structural grouping heuristics.
  #
  # These enrichments remain lightweight operational projections
  # derived from filesystem structure.
  # ------------------------------------------------------------------

  contextual_snapshot <- contextual_snapshot |>
    add_snapshot_context() |>
    add_structural_groups()

  # ------------------------------------------------------------------
  # RECORD SET PROJECTION
  # ------------------------------------------------------------------
  #
  # Create a lightweight contextual Record Set projection using
  # structural grouping heuristics derived from filesystem paths.
  #
  # This step does not construct authoritative archival Record Sets.
  # ------------------------------------------------------------------


  contextual_snapshot |>
    record_set_projection(
      record_set_id = "structural_group",
      resource_id = "inst_id",
      locator_path = "rel_root_path",
      construction_rule = c(
        "path_prefix",
        "structural_group"
      )
    )
}
