#' Read and combine observational filesystem snapshots
#'
#' Read one or more serialized observational filesystem snapshots and
#' combine them into a unified observational table.
#'
#' @description
#' `read_snapshot()` reconstructs an observational layer from one or
#' more snapshots previously created with [scan_storage()].
#'
#' The function preserves filesystem observations as originally recorded,
#' while appending snapshot-level provenance and contextual identifiers
#' that support longitudinal and cross-storage analytical workflows.
#'
#' The resulting table is intended to represent observed filesystem
#' Instantiations rather than authoritative documentary entities.
#'
#' @details
#' The function performs:
#'
#' - observational aggregation across snapshots
#' - snapshot-level provenance preservation
#' - contextual identifier enrichment
#' - optional materialization of repository-level Git metadata
#'
#' The function intentionally does not:
#'
#' - deduplicate observations
#' - infer stable file identity
#' - infer Record Resources or Record Sets
#' - resolve documentary semantics
#' - interpret provenance relationships
#'
#' Multiple observations of the same filesystem approximation may occur:
#'
#' - across observation times
#' - across storage contexts
#' - across partially overlapping snapshots
#' - across synchronized or copied working environments
#'
#' In RiC-aligned operational terms:
#'
#' - each row represents one observed filesystem Instantiation
#' - repeated observations may later support inference of more stable
#'   Record Resources
#' - higher-level documentary interpretation is deferred to later
#'   analytical or curatorial stages
#'
#' Snapshot-level provenance metadata are appended as columns to support:
#'
#' - provenance-aware analytics
#' - reconstruction workflows
#' - cross-storage comparison
#' - longitudinal temporal analysis
#'
#' Repository metadata are normally stored as snapshot attributes in
#' order to avoid repeating identical repository information across all
#' observations. When `include_repo_metadata = TRUE`, repository-level
#' metadata are materialized into the returned table to support
#' repository-aware analytical workflows.
#'
#' @param snapshot_files Character vector of snapshot `.rds` files.
#'
#' @param include_repo_metadata Logical.
#'
#' If `TRUE`, repository metadata stored in snapshot attributes are
#' materialized into the returned observational table.
#'
#' The following repository-level variables may be added:
#'
#' - `git_remote`
#' - `git_branch`
#' - `git_repo_id`
#'
#' This may increase memory usage because repository metadata are
#' repeated across all observations belonging to the same repository.
#'
#' @return A `data.frame` containing combined filesystem observations.
#'
#' The returned table contains all variables created by
#' [scan_storage()] together with additional provenance and contextual
#' identifiers:
#'
#' - `snapshot_file`:
#'   normalized path of the source snapshot artefact
#'
#' - `snapshot_created_at`:
#'   observation timestamp recorded in snapshot metadata
#'
#' - `snapshot_schema_version`:
#'   schema version recorded in snapshot metadata
#'
#' - `storage_full_path`:
#'   globally contextualized filesystem locator
#'   (`storage_id::full_path`)
#'
#' - `storage_path_id`:
#'   storage-scoped logical filesystem identifier
#'   (`storage_id::rel_path`)
#'
#' - `observation_id`:
#'   identifier of a specific filesystem observation event,
#'   combining storage context, logical path, and observation time
#'
#' @export

read_snapshot <- function(snapshot_files,
                          include_repo_metadata = FALSE) {
  if (!is.character(snapshot_files)) {
    stop(
      "snapshot_files must be a vector of characters containing file paths",
      call. = FALSE
    )
  }

  missing_snapshot_files <- snapshot_files[
    !file.exists(snapshot_files)
  ]

  if (length(missing_snapshot_files) > 0) {
    stop(
      "Snapshot files do not exist: ",
      paste(missing_snapshot_files, collapse = ", "),
      call. = FALSE
    )
  }

  snapshots <- purrr::map_dfr(snapshot_files, function(f) {
    x <- readRDS(f)

    if (!is.data.frame(x)) {
      stop(
        "Snapshot is not a data.frame: ",
        f,
        call. = FALSE
      )
    }

    # --- snapshot artefact provenance ---

    x$snapshot_file <- normalizePath(
      f,
      winslash = "/"
    )

    x$snapshot_created_at <- attr(
      x,
      "created_at"
    )

    x$snapshot_schema_version <- attr(
      x,
      "schema_version"
    )

    repos_df <- attr(x, "repos")

    if (
      include_repo_metadata &&
        !is.null(repos_df)
    ) {
      repos_df <- repos_df %>%
        mutate(
          git_repo_id = normalize_git_remote(git_remote)
        )

      x <- x %>%
        left_join(
          repos_df,
          by = "repo_root"
        )
    }

    # --- legacy compatibility ---
    # schema metadata was not consistently present
    # in earlier observational snapshots

    if (is.null(x$snapshot_schema_version)) {
      x$snapshot_schema_version <- "0.1.0"
    }

    x
  })

  # --- contextual observational identifiers ---
  # enriches observations for cross-storage and
  # longitudinal reconstruction workflows

  snapshots <- add_snapshot_context(snapshots)

  snapshots
}
