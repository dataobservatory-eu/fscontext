#' @title Create a provenance-aware Record Set data frame
#'
#' @description
#' Construct a `recordset_df`, a provenance-aware contextual dataset
#' representing members of a Record Set.
#'
#' `Record Set` is a contextual aggregation concept defined by the
#' International Council on Archives (ICA) Records in Contexts
#' standard (RiC). In operational terms, a Record Set may represent:
#'
#' - a project workspace;
#' - a research corpus;
#' - a synchronized working environment;
#' - a digital collection;
#' - a reconstruction context;
#' - or another contextual grouping of related digital resources.
#'
#' The Records in Contexts (RiC) standard provides a flexible and
#' provenance-aware approach for describing evolving digital records,
#' their relationships, and their contextual environments.
#'
#' Unlike rigid hierarchical archival models, RiC allows records and
#' digital resources to participate in multiple overlapping contextual
#' groupings while preserving provenance and contextual relationships.
#'
#' More information:
#'
#' - ICA Records in Contexts overview:
#'   \url{https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/}
#'
#' - RiC-O ontology repository:
#'   \url{https://github.com/ica-egad/ric-o}
#'
#' A `recordset_df` extends the
#' \code{\link[dataset:dataset_df]{dataset_df}} class with lightweight
#' contextual Record Set semantics suitable for:
#'
#' - filesystem observations;
#' - synchronized cloud folders;
#' - web archive members;
#' - digital surrogate collections;
#' - curation batches;
#' - Digital Twin workspaces;
#' - provenance-aware research collections;
#' - contextual digital preservation workflows.
#'
#' The class is designed to work together with:
#'
#' - [read_snapshot()]
#' - [snapshot_to_reconstruction_context()]
#' - [snapshot_to_recordset_df()]
#'
#' while preserving the distinction between:
#'
#' - observed filesystem evidence;
#' - contextual grouping of related resources;
#' - later analytical interpretation;
#' - and archival or semantic enrichment workflows.
#'
#' In operational terms:
#'
#' - `record_set_id`
#'   identifies a contextual grouping of related digital resources
#'   (similar to a project workspace, collection, or reconstruction
#'   environment);
#'
#' - `member_id`
#'   identifies one observed or asserted member within that grouping.
#'
#' The resulting object inherits from:
#'
#' - `recordset_df`
#' - `dataset_df`
#' - `tbl_df`
#' - `tbl`
#' - `data.frame`
#'
#' @param ...
#' Vectors (columns) to include in the record set.
#'
#' @param identifier
#' A named vector of URI prefixes used to generate row identifiers.
#'
#' Defaults to:
#'
#' `c(member = "http://example.com/recordset#member")`
#'
#' @param var_labels
#' Optional named list of human-readable variable labels.
#'
#' @param units
#' Optional named list of measurement units.
#'
#' @param concepts
#' Optional named list of semantic concept URIs.
#'
#' @param dataset_bibentry
#' Optional bibliographic metadata created with
#' \code{dataset::dublincore()} or
#' \code{dataset::datacite()}.
#'
#' @param dataset_subject
#' Optional dataset subject metadata.
#'
#' @return
#' A `recordset_df` object.
#'
#' @details
#' The constructor requires at minimum the columns:
#'
#' - `record_set_id`
#' - `member_id`
#'
#' Validation and class assignment are delegated to
#' \code{\link{new_recordset_df}}.
#'
#' The constructor is intentionally lightweight and does not:
#'
#' - infer authoritative archival hierarchy;
#' - reconcile duplicate identities;
#' - infer canonical resources;
#' - construct ontology-complete provenance graphs;
#' - or replace curatorial or archival interpretation.
#'
#' Instead, it provides a stable contextual preservation layer for
#' provenance-aware reconstruction and human-in-the-loop workflows.
#'
#' @examples
#' toy_recordset <- recordset_df(
#'   record_set_id = c(
#'     "heritage_digitisation",
#'     "heritage_digitisation",
#'     "heritage_digitisation"
#'   ),
#'   member_id = c(
#'     "inst_001",
#'     "inst_002",
#'     "inst_003"
#'   ),
#'   member_path = c(
#'     "scans/photo_001.tif",
#'     "ocr/photo_001.txt",
#'     "reports/collection_summary.qmd"
#'   ),
#'   member_type = c(
#'     "file",
#'     "file",
#'     "file"
#'   ),
#'   source_type = c(
#'     "filesystem",
#'     "filesystem",
#'     "filesystem"
#'   ),
#'   identifier = c(
#'     member =
#'       "https://example.org/recordset/heritage#member"
#'   ),
#'   var_labels = list(
#'     record_set_id = "Record set identifier",
#'     member_id = "Member identifier",
#'     member_path = "Member path"
#'   ),
#'   concepts = list(
#'     record_set_id =
#'       "https://www.ica.org/standards/RiC/ontology#RecordSet",
#'     member_id =
#'       "https://www.ica.org/standards/RiC/ontology#Instantiation"
#'   ),
#'   dataset_bibentry = dataset::dublincore(
#'     title = "Toy Heritage Digitisation Record Set",
#'     creator = person("Jane", "Doe"),
#'     publisher = "fscontext"
#'   )
#' )
#'
#' toy_recordset
#'
#' @export
recordset_df <- function(
  ...,
  identifier = c(
    member = "http://example.com/recordset#member"
  ),
  var_labels = NULL,
  units = NULL,
  concepts = NULL,
  dataset_bibentry = NULL,
  dataset_subject = NULL
) {
  x <- dataset::dataset_df(
    ...,
    identifier = identifier,
    var_labels = var_labels,
    units = units,
    concepts = concepts,
    dataset_bibentry = dataset_bibentry,
    dataset_subject = dataset_subject
  )

  new_recordset_df(x)
}

#' Internal constructor for `recordset_df`
#'
#' @description
#' Low-level internal constructor for creating `recordset_df` objects.
#'
#' This function:
#'
#' - validates required columns
#' - assigns the `recordset_df` class
#' - preserves existing classes
#'
#' Unlike \code{\link{recordset_df}}, this constructor does not create
#' semantic metadata structures or perform user-facing coercion.
#'
#' @param x
#' A data.frame or tibble containing at minimum:
#'
#' - `record_set_id`
#' - `member_id`
#'
#' @return
#' A `recordset_df` object.
#'
#' @keywords internal
new_recordset_df <- function(x) {
  stopifnot(is.data.frame(x))

  required_cols <- c(
    "record_set_id",
    "member_id"
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  class(x) <- unique(c(
    "recordset_df",
    class(x)
  ))

  x
}
