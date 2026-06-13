#' Create a contextual Record Set projection from observational resources
#'
#' @description
#' Constructs a lightweight contextual Record Set projection from an
#' observational resource table.
#'
#' `Record Set` is a contextual aggregation concept defined by the
#' International Council on Archives (ICA) Records in Contexts
#' standard (RiC).
#'
#' In operational terms, a Record Set may represent:
#'
#' - a project workspace;
#' - a synchronized cloud folder;
#' - a repository inventory;
#' - a digitisation batch;
#' - a web archive collection;
#' - a reconstruction corpus;
#' - or another contextual grouping of related digital resources.
#'
#' The function is designed as an operational bridge between:
#'
#' - filesystem observations;
#' - web archive inventories;
#' - digitised heritage collections;
#' - repository inventories;
#' - and later semantically enriched Record Set representations.
#'
#' The returned object is intentionally a plain tibble rather than a
#' semantically enriched `recordset_df`.
#'
#' This allows:
#'
#' - efficient tidyverse workflows;
#' - exploratory analytical pipelines;
#' - lightweight contextual reconstruction;
#' - deferred semantic stabilisation;
#' - provenance-aware iterative enrichment.
#'
#' More information:
#'
#' - ICA Records in Contexts overview:
#'   \url{https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/}
#'
#' - RiC-O ontology repository:
#'   \url{https://github.com/ica-egad/ric-o}
#'
#' In RiC-aligned operational terminology:
#'
#' - rows typically represent observed or derived Record Resources,
#'   Instantiations, or other operational resource proxies;
#'
#' - the resulting tibble represents a contextual Record Set projection
#'   constructed from deterministic operational rules;
#'
#' - the function does not create authoritative archival arrangement,
#'   fonds hierarchy, or curatorial description;
#'
#' - Record Set semantics remain analytical and operational unless
#'   later stabilised through curatorial or semantic workflows.
#'
#' Typical use cases include:
#'
#' - grouping filesystem observations into project-level Record Sets;
#' - constructing analytical corpora from repository structures;
#' - creating candidate archival aggregations;
#' - preparing Heritage Digital Twin analytical spaces;
#' - contextualising WARC/WACZ collections;
#' - constructing enrichment workspaces for knowledge-graph workflows.
#'
#' The function deliberately separates:
#'
#' - operational contextualisation (`create_record_set()`)
#'
#' from:
#'
#' - semantic publication and metadata enrichment
#'   (`as_recordset_df()`).
#'
#' This mirrors the package philosophy used throughout the observational
#' pipeline:
#'
#' - observe first;
#' - contextualise second;
#' - interpret later.
#'
#' @param x A `data.frame` or tibble containing observational or derived
#'   resource rows.
#'
#' @param record_set_id Character scalar or existing column name defining
#'   the contextual Record Set membership of each resource.
#'
#' Typical examples include:
#'
#' - structural filesystem groupings;
#' - repository roots;
#' - WARC collection identifiers;
#' - digitisation batches;
#' - curatorial aggregation identifiers.
#'
#' @param resource_id Character scalar or existing column name defining
#'   the operational identity of each resource within the Record Set.
#'
#' In many filesystem workflows, `resource_id` will often correspond
#' to what users informally think of as a "file" or "file name".
#'
#' However, the identifier intentionally represents an operational or
#' contextual resource approximation rather than an authoritative or
#' permanent file identity.
#'
#' This distinction matters because digital resources frequently evolve
#' over time:
#'
#' - filenames and paths may change;
#' - synchronized copies may diverge;
#' - local and cloud versions may coexist;
#' - files may be copied, renamed, or reorganised;
#' - multiple observations may refer to evolving versions of the
#'   same underlying resource.
#'
#' For example:
#'
#' - the same digital resource ("file") may exist in multiple locations;
#' - a synchronized cloud copy may differ from a local working copy;
#' - a renamed file may still represent the continuation of the same
#'   evolving digital resource.
#'
#' Typical examples include:
#'
#' - `storage_path_id`
#'   (storage-scoped filesystem resource approximation);
#'
#' - URI identifiers;
#'
#' - WARC record identifiers;
#'
#' - repository-relative identifiers;
#'
#' - IIIF resource identifiers.
#'
#' @param construction_rule Character description documenting the
#'   deterministic operational rule used to construct the contextual
#'   Record Set projection.
#'
#' Examples:
#'
#' - `"filtered_project_roots|structural_group"`
#' - `"warc_collection|domain_partition"`
#' - `"iiif_manifest|folder_batch"`
#'
#' The construction rule is stored as lightweight provenance metadata
#' attached to the resulting tibble.
#'
#' @param locator_path Optional character scalar or existing column name
#'   providing a human-readable operational locator associated with the
#'   resource.
#'
#' Examples include:
#'
#' - filesystem paths;
#' - repository-relative paths;
#' - URIs;
#' - WARC locators;
#' - IIIF resource paths.
#'
#' @param resource_title Optional character scalar or existing column name
#'   containing a human-readable resource title or label.
#'
#' @param resource_type Optional character scalar or existing column name
#'   describing the operational resource type.
#'
#' Examples:
#'
#' - `"file"`
#' - `"warc_record"`
#' - `"iiif_canvas"`
#' - `"rdf_resource"`
#' - `"digitised_page"`
#'
#' @return
#' A tibble representing a contextual operational Record Set projection.
#'
#' The resulting tibble contains:
#'
#' - `record_set_id`
#' - `resource_id`
#' - optional contextual resource variables
#'
#' together with lightweight provenance attributes:
#'
#' - `construction_rule`
#' - `created_by`
#' - `record_set_created_at`
#'
#' @details
#' The function intentionally performs only lightweight contextual
#' projection and validation.
#'
#' It does not:
#'
#' - infer authoritative documentary hierarchy;
#' - enforce archival arrangement;
#' - construct RiC-complete semantic graphs;
#' - perform provenance reasoning;
#' - stabilise resource identity across time.
#'
#' Semantic enrichment and publication-oriented metadata are intended
#' to be added later via `as_recordset_df()`.
#'
#' This staged architecture supports:
#'
#' - efficient analytical workflows;
#' - iterative reconstruction;
#' - provenance-aware contextualisation;
#' - future alignment with RiC-O and RiC-CM.
#'
#' @examples
#' toy_record_set <- tibble::tibble(
#'   structural_group = c(
#'     "heritage_collection",
#'     "heritage_collection",
#'     "digitisation_batch"
#'   ),
#'   storage_path_id = c(
#'     "laptop01::scans/photo_001.tif",
#'     "laptop01::ocr/photo_001.txt",
#'     "archive01::reports/summary.qmd"
#'   ),
#'   rel_root_path = c(
#'     "scans/photo_001.tif",
#'     "ocr/photo_001.txt",
#'     "reports/summary.qmd"
#'   )
#' )
#'
#' toy_record_set <- create_record_set(
#'   toy_record_set,
#'   record_set_id = "structural_group",
#'   resource_id = "storage_path_id",
#'   locator_path = "rel_root_path",
#'   construction_rule =
#'     "filtered_project_roots|structural_group",
#'   resource_type = "file"
#' )
#'
#' @importFrom dplyr mutate select all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom stats setNames
#'
#' @export

create_record_set <- function(
  x,
  record_set_id,
  resource_id,
  construction_rule,
  locator_path = NULL,
  resource_title = NULL,
  resource_type = NULL
) {
  stopifnot(is.data.frame(x))

  resolve_value <- function(data, value) {
    if (is.null(value)) {
      return(NULL)
    }

    if (
      is.character(value) &&
        length(value) == 1 &&
        value %in% names(data)
    ) {
      return(data[[value]])
    }

    rep(value, nrow(data))
  }

  out <- tibble::as_tibble(x)

  # --------------------------------------------------------------
  # Contextual record set projection
  # --------------------------------------------------------------

  out$record_set_id <-
    resolve_value(out, record_set_id)

  out$resource_id <-
    resolve_value(out, resource_id)

  out$locator_path <-
    resolve_value(out, locator_path)

  out$resource_title <-
    resolve_value(out, resource_title)

  out$resource_type <-
    resolve_value(out, resource_type)

  # --------------------------------------------------------------
  # Validation
  # --------------------------------------------------------------

  required_cols <- c(
    "record_set_id",
    "resource_id"
  )

  missing_required_cols <- setdiff(
    required_cols,
    names(out)
  )

  missing_required_values <- required_cols[
    required_cols %in% names(out) &
      vapply(
        out[required_cols[required_cols %in% names(out)]],
        function(col) all(is.na(col)),
        logical(1)
      )
  ]

  missing_cols <- c(
    missing_required_cols,
    missing_required_values
  )

  if (length(missing_cols) > 0) {
    stop(
      "Missing required values for: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --------------------------------------------------------------
  # Lightweight provenance
  # --------------------------------------------------------------

  attr(out, "construction_rule") <-
    construction_rule

  attr(out, "created_by") <-
    "create_record_set"

  attr(out, "record_set_created_at") <-
    Sys.time()

  out
}
