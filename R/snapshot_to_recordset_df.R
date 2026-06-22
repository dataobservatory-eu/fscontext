#' Create a contextual Record Set dataset
#'
#' @description
#' Creates a provenance-aware `recordset_df` from observational
#' filesystem snapshots and contextual reconstruction workflows.
#'
#' The function preserves observed filesystem resources while adding:
#'
#' - contextual Record Set assertions
#'   (human-defined grouping of related archived digital resources);
#'
#' - dataset-level provenance metadata
#'   (information about how, when, and from which observations
#'   the dataset was created);
#'
#' - preservation-oriented semantic context
#'   (structured contextual information supporting archival,
#'   audit, and long-term reconstruction workflows).
#'
#' Unlike [snapshot_to_reconstruction_context()], which is optimized
#' for analytical and forensic workflows, this function creates a
#' stable contextual preservation object suitable for:
#'
#' - contextual digital preservation;
#' - audit reconstruction;
#' - heritage and archival workflows;
#' - provenance-aware digital collections;
#' - human-in-the-loop semantic enrichment.
#'
#' @param snapshot_files Character vector of `.rds` snapshot files.
#'
#' @param roots Character vector of contextual root paths used
#'   for observational selection.
#'
#' @param record_set_id Character scalar giving the asserted
#'   identifier of the resulting Record Set.
#'
#' @param record_set_title Optional human-readable title.
#'
#' @param person A [utils::person()] object describing the creator
#'   of the semantic Record Set assertion.
#'
#' @param exclude_patterns Character vector of exclusion patterns
#'   passed to [subset_snapshot()].
#'
#' @return
#' A semantically enriched `recordset_df` object inheriting from
#' `dataset_df`.
#'
#' @details
#' The function intentionally reuses
#' [snapshot_to_reconstruction_context()] to preserve:
#'
#' - identical observational reconstruction logic;
#' - stable contextual identifiers;
#' - reproducible reconstruction workflows.
#'
#' The resulting object keeps observational rows intact while
#' adding a lightweight semantic preservation layer based on:
#'
#' - contextual Record Set assertions;
#' - provenance metadata;
#' - RiC-aligned contextual semantics.
#'
#' @references
#' International Council on Archives Expert Group on Archival Description
#' (2023). Records in Contexts (RiC).
#' <https://www.ica.org/ica-network/expert-groups/egad/records-in-contexts-ric/>
#'
#' @importFrom dplyr mutate select all_of
#' @importFrom dataset provenance provenance<-
#' @importFrom dataset dublincore subject_create n_triple n_triples
#' @importFrom utils person
#' @importFrom purrr map_chr
#' @export

snapshot_to_recordset_df <- function(
  snapshot_files,
  roots,
  record_set_id,
  record_set_title = NULL,
  person = utils::person("Jane", "Doe"),
  exclude_patterns = c("\\\\.Rcheck")
) {
  stopifnot(is.character(snapshot_files))
  stopifnot(is.character(roots))
  stopifnot(length(record_set_id) == 1)

  if (is.null(record_set_title)) {
    record_set_title <- paste0(
      "The ",
      record_set_id,
      " filesystem record set"
    )
  }

  # ------------------------------------------------------------
  # Contextual observational reconstruction
  # ------------------------------------------------------------

  recordset_df <- snapshot_to_reconstruction_context(
    snapshot_files = snapshot_files,
    roots = roots,
    exclude_patterns = exclude_patterns
  )

  # ------------------------------------------------------------
  # Human-defined Record Set assertion
  # ------------------------------------------------------------

  recordset_df$record_set_id <- record_set_id

  # ------------------------------------------------------------
  # Create dataset_df
  # ------------------------------------------------------------

  recordset_df <- dataset::dataset_df(
    recordset_df,
    identifier = c(
      obs = paste0(
        "https://fscontext.example.org/recordset/",
        record_set_id,
        "#"
      )
    ),
    dataset_bibentry = dataset::dublincore(
      title = record_set_title,
      creator = person,
      description = paste(
        "Filesystem-derived contextual record set",
        "created from observational filesystem snapshots."
      )
    ),
    dataset_subject = dataset::subject_create(
      term = "Record Set",
      valueURI = "https://www.ica.org/standards/RiC/ontology#RecordSet",
      subjectScheme = "RiC-O"
    )
  )

  class(recordset_df) <- c(
    "recordset_df",
    class(recordset_df)
  )

  # ------------------------------------------------------------
  # Provenance
  # ------------------------------------------------------------

  recordset_uri <- paste0(
    "https://fscontext.example.org/recordset/",
    record_set_id
  )

  activity_uri <- paste0(
    recordset_uri,
    "/activity/snapshot_to_recordset_df"
  )

  software_agent_uri <- paste0(
    "https://fscontext.example.org/software/snapshot_to_recordset_df"
  )

  given_name <- paste(person$given, collapse = "_")
  family_name <- paste(person$family, collapse = "_")

  person_uri <- paste0(
    "https://fscontext.example.org/agent/",
    given_name,
    "_",
    family_name
  )

  # ------------------------------------------------------------
  # Agent triples
  # ------------------------------------------------------------

  agent_triples <- c(
    dataset::n_triple(
      person_uri,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://xmlns.com/foaf/0.1/Person"
    ),
    dataset::n_triple(
      person_uri,
      "http://www.w3.org/2000/01/rdf-schema#label",
      paste(person$given, person$family)
    ),
    dataset::n_triple(
      software_agent_uri,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#SoftwareAgent"
    ),
    dataset::n_triple(
      activity_uri,
      "http://www.w3.org/ns/prov#wasAssociatedWith",
      software_agent_uri
    ),
    dataset::n_triple(
      activity_uri,
      "http://www.w3.org/ns/prov#wasAssociatedWith",
      person_uri
    )
  )

  # ------------------------------------------------------------
  # Typing triples
  # ------------------------------------------------------------

  typing_triples <- c(
    dataset::n_triple(
      recordset_uri,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#Entity"
    ),
    dataset::n_triple(
      activity_uri,
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#Activity"
    )
  )

  # ------------------------------------------------------------
  # Activity timing
  # ------------------------------------------------------------

  time_triples <- dataset::n_triple(
    activity_uri,
    "http://www.w3.org/ns/prov#endedAtTime",
    paste0(
      "\"",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      "\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"
    )
  )

  # ------------------------------------------------------------
  # Construction provenance
  # ------------------------------------------------------------

  construction_triples <- dataset::n_triple(
    recordset_uri,
    "http://www.w3.org/ns/prov#wasGeneratedBy",
    activity_uri
  )

  # ------------------------------------------------------------
  # Snapshot entities
  # ------------------------------------------------------------

  snapshot_entity_triples <- purrr::map_chr(
    snapshot_files,
    \(f) dataset::n_triple(
      paste0(
        "file:///",
        normalizePath(f, winslash = "/")
      ),
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      "http://www.w3.org/ns/prov#Entity"
    )
  )

  # ------------------------------------------------------------
  # Activity usage provenance
  # ------------------------------------------------------------

  usage_triples <- purrr::map_chr(
    snapshot_files,
    \(f) dataset::n_triple(
      activity_uri,
      "http://www.w3.org/ns/prov#used",
      paste0(
        "file:///",
        normalizePath(f, winslash = "/")
      )
    )
  )

  # ------------------------------------------------------------
  # Derivation provenance
  # ------------------------------------------------------------

  derivation_triples <- purrr::map_chr(
    snapshot_files,
    \(f) dataset::n_triple(
      recordset_uri,
      "http://www.w3.org/ns/prov#wasDerivedFrom",
      paste0(
        "file:///",
        normalizePath(f, winslash = "/")
      )
    )
  )

  # ------------------------------------------------------------
  # Attach provenance graph
  # ------------------------------------------------------------

  dataset::provenance(recordset_df) <- dataset::n_triples(c(
    agent_triples,
    typing_triples,
    time_triples,
    construction_triples,
    snapshot_entity_triples,
    usage_triples,
    derivation_triples
  ))

  recordset_df
}
