#' Classify operational file types from observed resources
#'
#' @description
#' Classifies observed digital resources into operational file types
#' using workflow-oriented classification profiles.
#'
#' Unlike simple MIME-type or extension lookups, the function is
#' designed for provenance-aware analytical and reconstruction
#' workflows where file meaning depends on operational context.
#'
#' The function supports lightweight operational classification for:
#'
#' - filesystem reconstruction;
#' - digital preservation review;
#' - repository analytics;
#' - synchronized workspace inspection;
#' - web archive inventories;
#' - and Heritage Digital Twin workflows.
#'
#' The current implementation provides a small set of built-in
#' profiles intended as operational starting points.
#'
#' These profiles are intentionally lightweight and extensible.
#'
#' Future versions may support:
#'
#' - user-defined profiles;
#' - YAML-based vocabularies;
#' - institutional review profiles;
#' - preservation-oriented classification schemes;
#' - workflow-specific semantic enrichment.
#'
#' The function is designed to work together with:
#'
#' - [read_snapshot()]
#' - [snapshot_to_reconstruction_context()]
#' - [create_record_set()]
#'
#' as part of layered provenance-aware reconstruction workflows.
#'
#' @param x A `data.frame` or tibble containing observed resources.
#'
#' @param extension Character scalar identifying the column containing
#'   file extensions.
#'
#' Defaults to `"extension"`.
#'
#' @param profile Character scalar defining the operational
#'   classification profile.
#'
#' Current built-in profiles include:
#'
#' - `"r_development"`
#'
#' The `"r_development"` profile is designed for:
#'
#' - R package development;
#' - Quarto and R Markdown workflows;
#' - reproducible research repositories;
#' - analytical reporting pipelines.
#'
#' @return
#' A character vector containing operational file type
#' classifications.
#'
#' Typical output categories include:
#'
#' - `"code"`
#' - `"markdown"`
#' - `"workspace"`
#' - `"data"`
#' - `"artifact"`
#' - `"document"`
#' - `"website_generated"`
#' - `"other"`
#'
#' @details
#' The function intentionally performs lightweight operational
#' classification only.
#'
#' It does not:
#'
#' - infer authoritative media types;
#' - inspect file contents;
#' - perform preservation risk assessment;
#' - infer documentary semantics;
#' - replace curatorial review.
#'
#' Classification is based primarily on operational workflow
#' heuristics derived from file extensions and workflow profiles.
#'
#' @examples
#' toy_files <- tibble::tibble(
#'   extension = c(
#'     "R",
#'     "qmd",
#'     "csv",
#'     "png",
#'     "woff2"
#'   )
#' )
#'
#' classify_operational_file_type(
#'   toy_files,
#'   profile = "r_development"
#' )
#'
#' @importFrom dplyr case_when
#' @export

classify_operational_file_type <- function(
  x,
  extension = "extension",
  profile = "r_development"
) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame or tibble",
      call. = FALSE
    )
  }

  if (!extension %in% names(x)) {
    stop(
      "Column not found: ",
      extension,
      call. = FALSE
    )
  }

  ext <- tolower(x[[extension]])

  dplyr::case_when(
    # ------------------------------------------------------------
    # R DEVELOPMENT PROFILE
    # ------------------------------------------------------------
    profile == "r_development" &
      ext %in% c("r") ~
      "code",
    profile == "r_development" &
      ext %in% c(
        "rmd",
        "qmd",
        "md"
      ) ~
      "markdown",
    profile == "r_development" &
      ext %in% c(
        "rdata",
        "rds"
      ) ~
      "workspace",
    profile == "r_development" &
      ext %in% c(
        "csv",
        "xlsx",
        "xls"
      ) ~
      "data",
    profile == "r_development" &
      ext %in% c(
        "png",
        "jpg",
        "jpeg",
        "webp"
      ) ~
      "artifact",
    profile == "r_development" &
      ext %in% c(
        "pdf",
        "docx"
      ) ~
      "document",
    profile == "r_development" &
      ext %in% c(
        "css",
        "js",
        "map",
        "woff",
        "woff2",
        "ttf"
      ) ~
      "website_generated",
    TRUE ~
      "other"
  )
}
