#' Summarise repeated and divergent filesystem observations
#'
#' Aggregates observed filesystem observations by filename and
#' lightweight content signatures (`quick_sig`).
#'
#' @details
#' The function identifies:
#'
#' - repeated identical observations;
#' - potentially synchronised copies;
#' - diverging versions of similarly named resources;
#' - distributed working duplicates.
#'
#' The function operates on observational filesystem evidence only.
#'
#' It does not:
#'
#' - infer authoritative file identity;
#' - establish Record Resource equivalence;
#' - reconstruct provenance lineage;
#' - determine curatorial relationships.
#'
#' In RiC-aligned operational terminology:
#'
#' - rows in the snapshot represent filesystem observations;
#'
#' - repeated identical `quick_sig` values provide operational
#'   evidence that multiple observations may correspond to the
#'   same underlying digital resource;
#'
#' - differing signatures associated with the same filename may
#'   indicate divergent versions, forks, or independently evolving
#'   resources.
#'
#' The function therefore supports:
#'
#' - longitudinal reconstruction;
#' - distributed workflow analysis;
#' - duplicate detection;
#' - exploratory Record Set construction;
#' - provenance-aware analytical workflows.
#'
#' Duplicate observations are not inherently anomalous.
#'
#' In distributed development workflows the same file may legitimately
#' appear:
#'
#' - across multiple machines;
#' - across synchronised project folders;
#' - in backup or staging locations;
#' - in derived analytical Record Sets.
#'
#' The function therefore reports observational duplication rather
#' than asserting erroneous copying.
#'
#' The function treats:
#'
#' - `filename` as a weak identity signal;
#' - `quick_sig` as a lightweight content equivalence signal.
#'
#' Missing signatures (`NA`) are treated as a valid observational group.
#'
#' This means:
#'
#' - multiple `NA` signatures are considered identical;
#' - a mix of `NA` and non-`NA` signatures counts as versioning.
#'
#' The function operates on observational snapshots and does not
#' resolve identity across time or storage contexts.
#'
#' @param df A snapshot `data.frame` conforming to the canonical
#'   snapshot schema created by [scan_storage()] or
#'   [read_snapshot()].
#'
#'   The dataset must contain:
#'
#'   - `filename`
#'   - `quick_sig` (may contain `NA`)
#'
#' @return
#' A `data.frame` with one row per `filename`.
#'
#' The returned variables include:
#'
#' \describe{
#'   \item{filename}{
#'     File basename used as grouping key.
#'   }
#'
#'   \item{total_copies}{
#'     Total number of observed filesystem occurrences.
#'   }
#'
#'   \item{identical_copies}{
#'     Size of the largest identical-signature group.
#'   }
#'
#'   \item{versioned_copies}{
#'     Number of observations outside the largest identical-signature
#'     group.
#'   }
#'
#'   \item{n_versions}{
#'     Number of distinct observed signatures.
#'   }
#' }
#'
#' @examples
#' data("fscontextdemo_snapshot_01")
#' data("fscontextdemo_snapshot_01")
#'
#' combined_snapshot <- rbind(
#'   fscontextdemo_snapshot_01,
#'   fscontextdemo_snapshot_01
#' )
#'
#' summarise_duplicates(combined_snapshot)
#'
#' @seealso [quick_signature()]
#'
#' @importFrom dplyr group_by summarise n n_distinct
#'
#' @export
summarise_duplicates <- function(df) {
  # --- strict schema check ---
  required <- c("filename", "quick_sig")
  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  df <- normalise_snapshot_schema(df)

  out <- df |>
    dplyr::group_by(filename) |>
    dplyr::summarise(
      total_copies = dplyr::n(),
      n_versions = dplyr::n_distinct(quick_sig, na.rm = FALSE),
      identical_copies = {
        tab <- table(quick_sig, useNA = "ifany")
        max_identical <- if (length(tab) > 0) max(tab) else 0
        if (max_identical > 1) max_identical else 0
      },
      versioned_copies = {
        tab <- table(quick_sig, useNA = "ifany")
        max_identical <- if (length(tab) > 0) max(tab) else 0

        if (dplyr::n_distinct(quick_sig, na.rm = FALSE) > 1) {
          dplyr::n() - max_identical
        } else {
          0
        }
      },
      .groups = "drop"
    )

  out <- out[, c(
    "filename",
    "total_copies",
    "identical_copies",
    "versioned_copies",
    "n_versions"
  )]

  as.data.frame(out, stringsAsFactors = FALSE)
}

#' @rdname summarise_duplicates
#' @export
summarize_duplicates <- summarise_duplicates
