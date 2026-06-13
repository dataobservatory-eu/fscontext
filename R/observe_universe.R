#' Construct a longitudinal observational universe
#'
#' Aggregates repeated filesystem observations into a lightweight
#' longitudinal observational universe.
#' 
#' @details
#' The function operates on snapshot `.rds` files created by
#' [scan_storage()] and summarises repeated observations of
#' operational filesystem aggregation units across time.
#'
#' The resulting table is intentionally observational and
#' pre-interpretive:
#'
#' - no intellectual Record Sets are inferred;
#' - no semantic reconciliation is performed;
#' - no provenance assertions beyond observation are made.
#'
#' Instead, the function provides a lightweight observational
#' universe suitable for:
#'
#' - reconstruction workflows;
#' - audit preparation;
#' - preservation planning;
#' - storage coverage analysis;
#' - identifying candidate contextual Record Sets;
#' - longitudinal filesystem observation.
#'
#' Observational aggregation units are operationally approximated
#' from observed file paths using configurable path truncation
#' rules.
#'
#' Aggregation units derived at different aggregation depths are
#' not directly comparable.
#'
#' Single files are never treated as aggregation units.
#'
#' Aggregation may optionally preserve:
#'
#' - storage boundaries (`storage_id`);
#' - person boundaries (`person_id`).
#'
#' @param snapshot_dir Directory containing snapshot `.rds` files.
#'
#' @param max_aggregation_depth Integer giving the maximum
#'   filesystem path depth used to derive observational
#'   aggregation units.
#'
#' @param by_storage Logical.
#'   If `TRUE`, aggregation preserves `storage_id`.
#'
#' @param by_person Logical.
#'   If `TRUE`, aggregation preserves `person_id`.
#'
#' @param exclude_patterns Character vector of regular
#'   expressions used to exclude operational artefacts
#'   from observational aggregation units.
#'
#'   Defaults exclude common:
#'
#'   - hidden metadata folders;
#'   - temporary artefacts;
#'   - generated build artefacts;
#'   - repository management files.
#'
#'   Exclusions are applied after aggregation-unit
#'   derivation and before longitudinal summarisation.
#'
#' @return
#' A tibble containing longitudinal observational summaries
#' of filesystem aggregation units.
#'
#' Variables include:
#'
#' \describe{
#'
#'   \item{observed_unit}{
#'   Operational filesystem aggregation unit derived from
#'   path truncation.
#'   }
#'
#'   \item{aggregation_depth}{
#'   Actual observed filesystem depth of the aggregation unit.
#'   }
#'
#'   \item{max_aggregation_depth}{
#'   Maximum filesystem path depth used during aggregation.
#'   }
#'
#'   \item{n_observations}{
#'   Number of snapshot observations in which the aggregation
#'   unit appeared.
#'   }
#'
#'   \item{avg_files_unit}{
#'   Average number of files observed per snapshot for the
#'   aggregation unit.
#'   }
#'
#'   \item{avg_size_unit}{
#'   Average observed size in bytes per snapshot for the
#'   aggregation unit.
#'   }
#'
#'   \item{avg_size_mb_unit}{
#'   Average observed size in megabytes per snapshot for the
#'   aggregation unit.
#'   }
#'
#'   \item{avg_size_gb_unit}{
#'   Average observed size in gigabytes per snapshot for the
#'   aggregation unit.
#'   }
#'
#'   \item{total_files_unit}{
#'   Total files observed for the aggregation unit across
#'   all snapshots.
#'   }
#'
#'   \item{total_size_unit}{
#'   Total bytes observed for the aggregation unit across
#'   all snapshots.
#'   }
#' }
#'
#' @examples
#' data("fscontextdemo_snapshot_01")
#' data("fscontextdemo_snapshot_02")
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#'
#' saveRDS(
#'   fscontextdemo_snapshot_01,
#'   file.path(tmp, "snapshot_01.rds")
#' )
#'
#' saveRDS(
#'   fscontextdemo_snapshot_02,
#'   file.path(tmp, "snapshot_02.rds")
#' )
#'
#' observation_universe <- observe_universe(
#'   snapshot_dir = tmp,
#'   max_aggregation_depth = 2
#' )
#'
#' head(observation_universe)
#' @importFrom dplyr mutate filter group_by summarise arrange across all_of
#' @importFrom purrr map_dfr map_chr
#' @importFrom tools file_ext
#' @importFrom magrittr %>%
#' @export

observe_universe <- function(
  snapshot_dir,
  max_aggregation_depth = 2,
  by_storage = TRUE,
  by_person = FALSE,
  exclude_patterns = c(
    "\\.gitignore$",
    "\\.Rbuildignore$",
    "\\.github$",
    "\\.quarto$",
    "\\.Rcheck$",
    "\\.RDataTmp",
    "\\.Trash-1000",
    "\\.cryptomator$",
    "\\.editorconfig$",
    "\\.gitattributes$",
    "\\.webmanifest$"
  )
) {
  if (!dir.exists(snapshot_dir)) {
    stop(
      paste(
        "snapshot_dir does not exist:",
        snapshot_dir
      ),
      call. = FALSE
    )
  }

  snapshot_files <- list.files(
    snapshot_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  if (length(snapshot_files) == 0) {
    stop(
      paste(
        "No snapshot files found in:",
        snapshot_dir
      ),
      call. = FALSE
    )
  }

  is_file_like <- function(x) {
    tools::file_ext(x) != ""
  }

  purrr::map_dfr(
    snapshot_files,
    function(f) {
      x <- tryCatch(
        readRDS(f),
        error = function(e) {
          stop(
            paste(
              "Failed to read snapshot file:",
              basename(f)
            ),
            call. = FALSE
          )
        }
      )

      if (!inherits(x, c("data.frame", "tbl_df"))) {
        stop(
          paste(
            "Snapshot file does not contain a data frame:",
            basename(f)
          ),
          call. = FALSE
        )
      }

      required_cols <- c(
        "full_path",
        "size"
      )

      missing_cols <- setdiff(
        required_cols,
        names(x)
      )

      if (length(missing_cols) > 0) {
        stop(
          paste(
            "Snapshot file is missing required columns:",
            paste(missing_cols, collapse = ", "),
            "| File:",
            basename(f)
          ),
          call. = FALSE
        )
      }

      x %>%
        dplyr::mutate(
          observed_unit =

            purrr::map_chr(
              full_path,
              function(p) {
                parts <- strsplit(
                  gsub("\\\\", "/", p),
                  "/"
                )[[1]]

                # ------------------------------------------------------
                # Remove filename before aggregation
                # ------------------------------------------------------

                if (
                  is_file_like(
                    parts[length(parts)]
                  )
                ) {
                  parts <- parts[-length(parts)]
                }

                # ------------------------------------------------------
                # Separate drive from folders
                # ------------------------------------------------------

                drive <- parts[1]

                folders <- parts[-1]

                # ------------------------------------------------------
                # Aggregation depth applies after drive
                # ------------------------------------------------------

                kept_folders <-
                  folders[
                    seq_len(
                      min(
                        length(folders),
                        max_aggregation_depth
                      )
                    )
                  ]

                unit <- paste(
                  c(drive, kept_folders),
                  collapse = "/"
                )

                normalize_context_roots(unit)
              }
            ),
          aggregation_depth =
            aggregation_depth(observed_unit),
          snapshot_file =
            basename(f),
          max_aggregation_depth =
            max_aggregation_depth
        ) %>%
        dplyr::filter(
          observed_unit != "."
        ) %>%
        dplyr::filter(
          !grepl(
            paste(
              exclude_patterns,
              collapse = "|"
            ),
            observed_unit
          )
        )
    }
  ) %>%
    {
      grouping_vars <- c(
        if (by_person) {
          "person_id"
        },
        if (by_storage) {
          "storage_id"
        },
        "aggregation_depth",
        "max_aggregation_depth",
        "observed_unit"
      )

      group_by(
        .,
        across(all_of(grouping_vars))
      )
    } %>%
    summarise(
      n_observations =
        n_distinct(snapshot_file),
      avg_files_unit =
        round(
          n() / n_observations,
          1
        ),
      avg_size_unit =
        round(
          sum(size, na.rm = TRUE) /
            n_observations,
          0
        ),
      avg_size_mb_unit =
        round(
          avg_size_unit / 1024^2,
          2
        ),
      avg_size_gb_unit =
        round(
          avg_size_unit / 1024^3,
          2
        ),
      total_files_unit =
        n(),
      total_size_unit =
        sum(size, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(
      desc(n_observations),
      desc(avg_size_unit)
    )
}
