#' Project observational resources into a Record Set layout
#'
#' @description
#' Internal helper that projects observational resources into the
#' canonical tabular layout used by `recordset_df()`.
#'
#' The function standardises contextual variables produced during
#' observational reconstruction by creating the operational columns
#' expected by the Record Set data model.
#'
#' It performs only lightweight projection and validation. No semantic
#' metadata, RiC assertions, or provenance graph are created.
#'
#' @param x A data frame containing observational or derived resources.
#'
#' @param record_set_identifier Character scalar or existing column name
#' defining the contextual Record Set to which each resource belongs.
#'
#' @param resource_id Character scalar or existing column name defining
#' the identifier of each resource.
#'
#' @param construction_rule Optional description of the deterministic
#' rule used to derive the Record Set projection. Stored as an attribute
#' of the returned tibble.
#'
#' @param locator_path Optional character scalar or existing column name
#' giving a human-readable locator for each resource.
#'
#' @param resource_title Optional character scalar or existing column
#' name containing a human-readable resource title.
#'
#' @param resource_type Optional character scalar or existing column
#' name describing the operational resource type.
#'
#' @return
#' A tibble containing the canonical operational Record Set variables,
#' including:
#'
#' \describe{
#'   \item{record_set_identifier}{Contextual Record Set identifier.}
#'   \item{resource_id}{Operational resource identifier.}
#'   \item{locator_path}{Optional resource locator.}
#'   \item{resource_title}{Optional resource label.}
#'   \item{resource_type}{Optional resource type.}
#' }
#'
#' The returned object also retains the `construction_rule` attribute
#' when supplied.
#'
#' @details
#' This function is an internal step in the fscontext reconstruction
#' pipeline:
#'
#' \preformatted{
#' filesystem observations
#'          ↓
#' derive_record_set()
#'          ↓
#' record_set_projection()
#'          ↓
#' recordset_df()
#' }
#'
#' `record_set_projection()` standardises an operational reconstruction.
#' Semantic metadata, provenance and RiC-oriented annotations are added
#' later by `recordset_df()`.
#'
#' Typical use:
#'
#' \preformatted{
#' toy_record_set <- tibble::tibble(
#'   structural_group = c("heritage_collection",
#'                        "heritage_collection"),
#'   storage_path_id = c("a", "b"),
#'   rel_root_path = c("a.html", "b.html")
#' )
#'
#' toy_record_set <- record_set_projection(
#'   toy_record_set,
#'   record_set_identifier = "structural_group",
#'   resource_id = "storage_path_id",
#'   locator_path = "rel_root_path"
#' )
#' }
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr mutate select all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom stats setNames

record_set_projection <- function(
  x,
  record_set_identifier,
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

  out$record_set_identifier <-
    resolve_value(out, record_set_identifier)

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
    "record_set_identifier",
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

  attr(out, "construction_rule") <- construction_rule


  out
}
