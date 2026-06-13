#' Create an observational grouping from filesystem snapshots
#'
#' Loads one or more filesystem snapshots, binds them together,
#' and assigns each observed instantiation to the nearest matching
#' observational root.
#'
#' This function is intentionally observational and lightweight:
#'
#' - no semantic inference
#' - no RiC interpretation
#' - no derivation logic
#' - no rename detection
#'
#' It creates a stable operational grouping layer that can later
#' support:
#'
#' - audit reconstruction
#' - duplicate analysis
#' - Git enrichment
#' - RiC-aligned record set derivation
#' - provenance graph construction
#'
#' @param snapshot_files Character vector of `.rds` snapshot paths.
#' @param roots Character vector of observation roots.
#' @param construction_method Character scalar describing how
#'   observational grouping was created.
#'
#' @return
#' A data.frame with observational grouping columns:
#'
#' - `observation_root`
#' - `rel_observation_path`
#' - `construction_method`
#'
#' @examples
#' \dontrun{
#'
#' observation_set <- create_observation_set(
#'   snapshot_files = snapshot_files,
#'   roots = eviota_roots
#' )
#' }
#'
#' @export

create_observation_set <- function(
  snapshot_files,
  roots,
  construction_method = "path_prefix"
) {
  stopifnot(is.character(snapshot_files))
  stopifnot(length(snapshot_files) > 0)

  stopifnot(is.character(roots))
  stopifnot(length(roots) > 0)

  stopifnot(is.character(construction_method))
  stopifnot(length(construction_method) == 1)

  # ------------------------------------------------------------------
  # LOAD SNAPSHOTS
  # ------------------------------------------------------------------

  observation_set <- purrr::map_dfr(
    snapshot_files,
    readRDS
  )

  # ------------------------------------------------------------------
  # NORMALIZE ROOTS
  # ------------------------------------------------------------------

  roots <- fs::path_abs(roots)

  observation_set <- observation_set |>
    dplyr::mutate(
      full_path = fs::path_abs(full_path)
    )

  # ------------------------------------------------------------------
  # FIND NEAREST MATCHING ROOT
  # ------------------------------------------------------------------

  find_observation_root <- function(path, roots) {
    matches <- roots[startsWith(path, roots)]

    if (length(matches) == 0) {
      return(NA_character_)
    }

    matches[which.max(nchar(matches))]
  }

  observation_root <- purrr::map_chr(
    observation_set$full_path,
    find_observation_root,
    roots = roots
  )

  # ------------------------------------------------------------------
  # ATTACH OBSERVATIONAL CONTEXT
  # ------------------------------------------------------------------

  observation_set <- observation_set |>
    dplyr::mutate(
      observation_root = observation_root
    ) |>
    dplyr::filter(
      !is.na(observation_root)
    ) |>
    dplyr::mutate(
      rel_observation_path = fs::path_rel(
        full_path,
        start = observation_root
      ),
      construction_method = construction_method
    )

  # ------------------------------------------------------------------
  # PROVENANCE
  # ------------------------------------------------------------------

  attr(observation_set, "created_at") <- Sys.time()

  attr(observation_set, "created_by") <-
    "create_observation_set"

  attr(observation_set, "construction_method") <-
    construction_method

  class(observation_set) <- c(
    "observation_set_df",
    class(observation_set)
  )

  observation_set
}
