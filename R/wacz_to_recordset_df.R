#' Create a Record Set dataset from a WACZ observation
#'
#' @description
#' Converts a `wacz_observation` created with [observe_wacz()] into a
#' semantically enriched `dataset_df` representing a Record Set.
#'
#' The function preserves the original observations while attaching
#' dataset-level metadata and lightweight Records in Contexts (RiC)
#' semantics. Selected identifier columns may be declared as
#' `rico:Identifier` values, allowing downstream workflows to distinguish
#' identifiers intended to refer to Records or Record Parts without
#' requiring a complete RiC-O implementation.
#'
#' The function intentionally performs only lightweight semantic
#' enrichment. It does not infer Records, Record Parts, Instantiations,
#' or other archival entities, nor does it reconcile identities or build
#' provenance graphs. Such interpretation is expected to occur in later
#' human-guided curation or semantic stabilisation workflows.
#'
#' @param wacz_observation
#' A `wacz_observation` object created with [observe_wacz()].
#'
#' @param record_set_id
#' Optional identifier for the resulting Record Set. If `NULL`, the
#' basename of the WACZ archive (without extension) is used.
#'
#' @param record_set_title
#' Optional human-readable title for the Record Set. If omitted, a title
#' is constructed automatically.
#'
#' @param record_identifier
#' Name of the column whose values identify Records represented in the
#' Record Set. The selected column is annotated as
#' `rico:Identifier` using [dataset::defined()]. Set to `NULL` to skip
#' annotation.
#'
#' @param record_part_identifier
#' Optional name of a column whose values identify Record Parts. The
#' selected column is annotated as `rico:Identifier`.
#'
#' @param person
#' A [utils::person()] object describing the creator of the resulting
#' dataset metadata.
#'
#' @return
#' A `dataset_df` object enriched with:
#'
#' * Dublin Core dataset metadata;
#' * a RiC Record Set subject;
#' * optional semantic annotations for Record and Record Part identifiers;
#' * the original `datapackage` and `wacz` attributes.
#'
#' @details
#' This function occupies the boundary between observational data and
#' semantic interpretation.
#'
#' `observe_wacz()` records observations extracted from a WACZ archive.
#' `wacz_to_recordset_df()` adds curatorial assertions describing how
#' particular observed identifiers should be interpreted within a Record
#' Set, while deliberately avoiding stronger ontological commitments such
#' as identity reconciliation or Record construction.
#'
#' The resulting object is intended for reproducible archival,
#' curatorial, and semantic enrichment workflows.
#'
#' @references
#' International Council on Archives Expert Group on Archival Description
#' (2023). Records in Contexts (RiC).
#' <https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/>
#'
#' @seealso
#' [observe_wacz()], [dataset::dataset_df()], [dataset::defined()]
#'
#' @export

wacz_to_recordset_df <- function(
    wacz_observation,
    record_set_id = NULL,
    record_set_title = NULL,
    record_identifier = "resource_locator",
    record_part_identifier = NULL,
    person = utils::person("Jane", "Doe")
) {
  
  wacz <- attr(wacz_observation, "wacz")
  
  if (
    is.null(wacz) ||
    !is.character(wacz) ||
    length(wacz) != 1 ||
    !grepl("\\.wacz$", wacz, ignore.case = TRUE)
  ) {
    stop(
      "`wacz_observation` must be created with observe_wacz().",
      call. = FALSE
    )
  }
  
  if (is.null(record_set_id)) {
    record_set_id <- tools::file_path_sans_ext(
      basename(wacz)
    )
  }
  
  if (is.null(record_set_title)) {
    record_set_title <- paste(
      "WACZ Record Set:",
      record_set_id
    )
  }
  
  if (!is.null(record_identifier) && !record_identifier%in% names(wacz_observation)) {
    stop(
      "`record_identifier` must name a column in `wacz_observation`.",
      call. = FALSE
    )
  }
  
  if (!is.null(record_part_identifier) &&
      !record_part_identifier %in% names(wacz_observation)) {
    stop(
      "`record_part_identifier` must name a column in `wacz_observation`.",
      call. = FALSE
    )
  }
 
  
  recordset_df <- dataset::dataset_df(
    wacz_observation,
    dataset_bibentry = dataset::dublincore(
      title = record_set_title,
      creator = person,
      description = paste(
        "Record Set created from the WACZ web archive",
        basename(wacz)
      )))

  dataset::subject(recordset_df) <- dataset::subject_create(
    term = "Record Set",
    valueURI = "https://www.ica.org/standards/RiC/ontology#RecordSet",
    subjectScheme = "RiC-O"
  )
    

  if(!is.null(record_identifier) && (record_identifier%in% names(recordset_df))) {
    
    tmp <- recordset_df[[record_identifier]]
    
    if (anyDuplicated(tmp)) {
      warning(
        "Record identifiers are not unique.",
        call. = FALSE
      )
    }
    
    recordset_df[[record_identifier]] <- dataset::defined(tmp, 
                     label = "Record Identifier", 
                     concept = "rico:Identifier")
  
  }
  
  if(!is.null(record_part_identifier) && 
     (record_part_identifier %in% names(recordset_df))
     ) {
    
    tmp <- recordset_df[[record_part_identifier]]
    
    if (anyDuplicated(tmp)) {
      warning(
        "Record part identifiers are not unique.",
        call. = FALSE
      )
    }
    
    recordset_df[[record_part_identifier]] <- dataset::defined(
      tmp, 
      label = "Record Part Identifier", 
      concept = "rico:Identifier")
    
  }
  
  if(!is.null(record_set_id) && !is.na(record_set_id)) {
    dataset::identifier(recordset_df) <- record_set_id
  }
  
  attr(recordset_df, "datapackage") <- attr(wacz_observation, "datapackage")
  attr(recordset_df, "wacz") <- wacz
  
  recordset_df
}


