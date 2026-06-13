#' Coerce a contextual Record Set projection into a semantically enriched
#' `recordset_df`
#'
#' Converts a contextual Record Set projection created with
#' [create_record_set()] into a semantically enriched `recordset_df`
#' object.
#'
#' The function adds lightweight dataset-level semantics and publication
#' metadata while preserving tidyverse compatibility.
#'
#' This staged design deliberately separates:
#'
#' - operational contextualisation (`create_record_set()`)
#'
#' from:
#'
#' - semantic stabilisation and publication (`as_recordset_df()`)
#'
#' The resulting object aligns with the package philosophy of:
#'
#' - observational acquisition,
#' - contextual enrichment,
#' - deferred semantic interpretation.
#'
#' `as_recordset_df()` is conceptually aligned with:
#'
#' - RiC-O Record Set projections,
#' - contextual research workspaces,
#' - analytical Heritage Digital Twin layers,
#' - and semantically enriched reconstruction corpora.
#'
#' The function builds on the `dataset_df` framework and therefore
#' inherits:
#'
#' - tibble semantics,
#' - lightweight dataset metadata,
#' - publication-oriented enrichment,
#' - and future linked-data extensibility.
#'
#' The function also acts as a lightweight semantic alignment layer
#' between:
#'
#' - operational resource-oriented contextualisation
#'
#' and:
#'
#' - semantically stabilised record set member representations.
#'
#' Operational columns are mapped into the opinionated
#' `recordset_df` vocabulary:
#'
#' - `resource_id` → `member_id`
#' - `locator_path` → `member_path`
#' - `resource_type` → `member_type`
#'
#' by default, although alternative mappings may be supplied.
#'
#' @param x A tibble or `data.frame`, typically created with
#'   [create_record_set()].
#'
#' @param title Human-readable title of the contextual Record Set.
#'
#' @param creator Creator metadata passed to
#'   [dataset::dublincore()].
#'
#' @param member_id Character scalar giving the column name in `x`
#'   that should be mapped to `member_id` in the resulting
#'   `recordset_df`.
#'
#' Defaults to `"resource_id"`.
#'
#' @param member_path Optional character scalar giving the column name
#'   in `x` that should be mapped to `member_path`.
#'
#' Defaults to `"locator_path"`.
#'
#' @param member_type Optional character scalar giving the column name
#'   in `x` that should be mapped to `member_type`.
#'
#' Defaults to `"resource_type"`.
#'
#' @param description Optional textual description documenting the
#'   contextual scope, construction logic, provenance assumptions,
#'   or analytical purpose of the Record Set.
#'
#' @param publisher Optional publisher metadata passed to
#'   [dataset::dublincore()].
#'
#' @param subject Optional subject metadata for future semantic
#'   enrichment.
#'
#' @return
#' A semantically enriched `recordset_df` object inheriting from:
#'
#' - `recordset_df`
#' - `dataset_df`
#' - `tbl_df`
#'
#' @details
#' The function creates lightweight semantic metadata but intentionally
#' avoids:
#'
#' - authoritative archival description,
#' - full RiC-O graph construction,
#' - provenance reasoning,
#' - or ontology-complete archival modelling.
#'
#' This lightweight semantic layer is intended for:
#'
#' - analytical reconstruction,
#' - contextual reporting,
#' - HDTO-like analytical workspaces,
#' - and iterative semantic enrichment workflows.
#'
#' @examples
#' toy_record_set <- tibble::tibble(
#'   structural_group = c(
#'     "_packages/eviota",
#'     "_packages/eviota",
#'     "_packages/iotables"
#'   ),
#'   path_id = c(
#'     "l480::R/import.R",
#'     "l480::data-raw/build.R",
#'     "l480::R/cube.R"
#'   ),
#'   rel_root_path = c(
#'     "R/import.R",
#'     "data-raw/build.R",
#'     "R/cube.R"
#'   )
#' )
#'
#' toy_record_set <- toy_record_set |>
#'   create_record_set(
#'     record_set_id = "structural_group",
#'     resource_id = "path_id",
#'     locator_path = "rel_root_path",
#'     construction_rule =
#'       "filtered_project_roots|structural_group",
#'     resource_type = "file"
#'   ) |>
#'   as_recordset_df(
#'     title = "Toy reconstruction workspace",
#'     creator = person("Daniel", "Antal"),
#'     description =
#'       "Contextual reconstruction record set"
#'   )
#'
#' @importFrom tibble as_tibble
#' @importFrom dataset dublincore
#' @importFrom utils person
#'
#' @export
as_recordset_df <- function(
  x,
  title,
  creator,
  member_id = "resource_id",
  member_path = "locator_path",
  member_type = "resource_type",
  description = NULL,
  publisher = NULL,
  subject = NULL
) {
  stopifnot(is.data.frame(x))

  x <- tibble::as_tibble(x)

  # --------------------------------------------------------------
  # Semantic alignment
  # --------------------------------------------------------------

  if (!member_id %in% names(x)) {
    stop(
      "Column not found: ",
      member_id,
      call. = FALSE
    )
  }

  names(x)[names(x) == member_id] <-
    "member_id"

  if (
    !is.null(member_path) &&
      member_path %in% names(x)
  ) {
    names(x)[names(x) == member_path] <-
      "member_path"
  }

  if (
    !is.null(member_type) &&
      member_type %in% names(x)
  ) {
    names(x)[names(x) == member_type] <-
      "member_type"
  }

  # --------------------------------------------------------------
  # Semantic metadata
  # --------------------------------------------------------------


  # Create the record set construction rules as a description

  construction_rule <-
    attr(x, "construction_rule")

  full_description <- description

  if (!is.null(construction_rule)) {
    rule_text <- paste(
      "Construction rule:",
      construction_rule
    )

    if (is.null(full_description)) {
      full_description <- rule_text
    } else {
      full_description <- paste(
        full_description,
        "",
        rule_text,
        sep = "\n"
      )
    }
  }

  bibentry <- dataset::dublincore(
    title = title,
    description = full_description,
    creator = creator,
    publisher = publisher
  )

  # --------------------------------------------------------------
  # Construct semantically enriched recordset_df
  # --------------------------------------------------------------

  out <- do.call(
    recordset_df,
    c(
      as.list(x),
      list(
        dataset_bibentry = bibentry,
        dataset_subject = subject
      )
    )
  )

  out
}
