#' @title Create a semantically annotated Record Set
#'
#' @description
#' Create a `recordset_df`, a lightweight extension of
#' `dataset::dataset_df` for representing archival Record Sets.
#'
#' A `recordset_df` preserves ordinary tabular data while allowing
#' selected columns to be declared as identifiers of RiC Records and
#' Record Parts. It is intended for provenance-aware archival,
#' curatorial and semantic enrichment workflows without requiring a
#' complete implementation of the Records in Contexts (RiC) ontology.
#'
#' See the **recordset_df** vignette for a complete workflow starting
#' from filesystem observations.
#'
#' @param x A `data.frame` or `dataset_df`.
#'
#' @param title Character scalar giving the title of the Record Set.
#'
#' @param creator A `utils::person()` object describing the creator of
#'   the Record Set metadata.
#'
#' @param description Optional description of the Record Set.
#'
#' @param record_set_identifier Optional identifier of the Record Set.
#'
#' @param record_identifier Name of the column containing Record
#'   identifiers. The selected column is annotated as
#'   `rico:Identifier` and labelled "Record Identifier".
#'
#' @param record_part_identifier Name of the column containing Record
#'   Part identifiers. The selected column is annotated as
#'   `rico:Identifier` and labelled "Record Part Identifier".
#'
#' @param record_subject Subject term describing the Record Set.
#'   Defaults to `"Record Set"`.
#'
#' @param ... Reserved for future extensions.
#'
#' @return
#' A `recordset_df`, which inherits from `dataset_df`, `tbl_df`,
#' `tbl` and `data.frame`.
#'
#' @examples
#' x <- data.frame(
#'   resource_locator = c(
#'     "https://example.org/1",
#'     "https://example.org/2"
#'   ),
#'   filename = c(
#'     "a.html",
#'     "b.html"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' rs <- recordset_df(
#'   x,
#'   title = "Demo Record Set",
#'   creator = utils::person("Joe", "Doe", role = "aut"),
#'   record_identifier = "resource_locator",
#'   record_part_identifier = "filename"
#' )
#'
#' rs
#'
#' @references
#' International Council on Archives Expert Group on Archival
#' Description (2023). Records in Contexts (RiC).
#' https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/
#'
#' @seealso
#' [dataset::dataset_df()], [observe_wacz()],
#' [wacz_to_recordset_df()]
#'
#' @importFrom dataset as_dataset_df defined dublincore subject identifier
#' @importFrom utils person
#' @export
recordset_df <- function(
  x,
  title = NULL,
  creator = utils::person("Jane", "Doe"),
  description = NULL,
  record_set_identifier = NULL,
  record_identifier = NULL,
  record_part_identifier = NULL,
  record_subject = "Record Set",
  ...
) {
  if (is.null(creator)) {
    rs_creator <- "Untitled Record Set"
  } else if (!inherits(creator, "person")) {
    rs_creator <- person(creator)
  } else {
    rs_creator <- creator
  }

  if (is.null(description) || is.na(description)) {
    rs_description <- NULL
  } else {
    rs_description <- description
  }

  ds_bibentry <- dataset::dublincore(
    title = if (is.null(title)) "Untitled Record Set" else title,
    identifier = record_set_identifier,
    creator = creator,
    description = description
  )

  ds_bibentry

  y <- dataset::as_dataset_df(x)

  attr(y, "dataset_bibentry") <- ds_bibentry

  dataset::subject(y) <-
    dataset::subject_create(term = record_subject)


  new_recordset_df(
    y,
    record_identifier = record_identifier,
    record_part_identifier = record_part_identifier
  )
}

#' Internal constructor for recordset_df
#'
#' @description
#' Low-level constructor for recordset_df objects.
#'
#' This function extends an existing dataset_df with lightweight
#' Record Set semantics by:
#'
#' * assigning the recordset_df class;
#' * optionally assigning a Record Set identifier and provenance;
#' * optionally declaring Record and Record Part identifier columns as
#' rico:Identifier using [dataset::defined()].
#'
#' Unlike [recordset_df()], this constructor assumes that dataset-level
#' metadata have already been created and performs no coercion from
#' ordinary data.frame objects.
#'
#' @param x A dataset_df object.
#'
#' @param record_set_identifier Optional identifier of the Record Set.
#'
#' @param record_identifier Optional name of the column containing
#' Record identifiers.
#'
#' @param record_part_identifier Optional name of the column containing
#' Record Part identifiers.
#'
#' @return
#' A recordset_df object inheriting from dataset_df.
#'
#' @importFrom dataset identifier
#' @keywords internal
new_recordset_df <- function(
  x,
  record_identifier = NULL,
  record_part_identifier = NULL
) {
  if (!inherits(x, "dataset_df")) {
    stop("`x` must inherit from dataset_df.", call. = FALSE)
  }

  record_set_identifier <- dataset::identifier(x)

  dataset::provenance(x) <- recordset_provenance(record_set_identifier)

  if (!is.null(record_identifier)) {
    if (!record_identifier %in% names(x)) {
      stop("Column not found: ", record_identifier, call. = FALSE)
    }

    if (anyDuplicated(x[[record_identifier]])) {
      warning("Record identifiers are not unique.", call. = FALSE)
    }

    x[[record_identifier]] <- dataset::defined(
      x[[record_identifier]],
      label = "Record Identifier",
      concept = "rico:Identifier"
    )
  }

  if (!is.null(record_part_identifier)) {
    if (!record_part_identifier %in% names(x)) {
      stop("Column not found: ", record_part_identifier, call. = FALSE)
    }

    if (anyDuplicated(x[[record_part_identifier]])) {
      warning("Record part identifiers are not unique.", call. = FALSE)
    }

    x[[record_part_identifier]] <- dataset::defined(
      x[[record_part_identifier]],
      label = "Record Part Identifier",
      concept = "rico:Identifier"
    )
  }

  class(x) <- unique(c("recordset_df", class(x)))


  attr(x, "prov") <-
    recordset_provenance(record_set_identifier = record_set_identifier)

  x
}

#' Default internal provenance constructor
#' A wrapper around [dataset::n_triples()] and [dataset::n_triple()].
#' @keywords internal
#' @importFrom dataset n_triples n_triple
#' @noRd
recordset_provenance <- function(
  record_set_identifier = NULL
) {
  if (is.null(record_set_identifier)) {
    record_set_identifier <- "http://example.com/recordset"
  }


  activity <- paste0(record_set_identifier, "/activity")

  dataset::n_triples(c(
    dataset::n_triple(
      record_set_identifier,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#Entity"
    ),
    dataset::n_triple(
      activity,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#Activity"
    ),
    dataset::n_triple(
      "https://fscontext.dataobservatory.eu/software/fscontext",
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#SoftwareAgent"
    ),
    dataset::n_triple(
      activity,
      "http://www.w3.org/ns/prov#wasAssociatedWith",
      "https://fscontext.dataobservatory.eu/software/fscontext"
    ),
    dataset::n_triple(
      record_set_identifier,
      "http://www.w3.org/ns/prov#wasGeneratedBy",
      activity
    )
  ))
}
