#' Classify operational file types
#'
#' Classify observed resources into operational categories that support
#' contextual reconstruction and Record Set derivation.
#'
#' @description
#' `classify_operational_file_type()` assigns observed files to
#' broad operational categories such as code, data, documents,
#' generated website files, and other artefacts.
#'
#' The current implementation provides an `"r_development"` profile
#' derived from software development, reproducible research,
#' and analytical repository workflows.
#'
#' The broader objective of operational classification is to
#' introduce an intermediate semantic layer between filesystem
#' observations and contextual reconstruction.
#'
#' @details
#' In many digital collections, archives, and research
#' environments, files participate in operational roles that
#' cannot be inferred from directory structure alone. Examples
#' include source materials, preservation masters, derivative
#' artefacts, metadata records, rights documentation, analytical
#' outputs, and publication-ready resources.
#'
#' Operational classification provides a lightweight mechanism
#' for assigning observed resources to such workflow-oriented
#' categories before higher-level contextualisation, Record Set
#' construction, or semantic stabilisation takes place.
#'
#' Classification is currently based on extension patterns and
#' workflow-specific heuristics. The function does not inspect
#' file contents, infer authoritative media types, determine
#' archival significance, or perform provenance reasoning.
#'
#' The resulting classifications should therefore be interpreted
#' as operational hypotheses that support exploration,
#' reconstruction, and contextualisation workflows rather than
#' authoritative documentary assertions.
#'
#' Future classification profiles may support archival,
#' audiovisual, heritage, research-data, and Records in Contexts
#' workflows, where operational roles provide an important bridge
#' between low-level filesystem observations and higher-level
#' documentary interpretation.
#'
#' @param x A `data.frame` or tibble containing observed files.
#'
#' @param extension Character scalar identifying the column that
#'   contains file extensions. Defaults to `"extension"`.
#'
#' @param profile Character scalar defining the classification
#'   profile. Currently only `"r_development"` is implemented.
#'
#' @return
#' A character vector of operational file type labels.
#'
#' Possible values for the `"r_development"` profile include:
#'
#' \describe{
#'   \item{code}{R source files.}
#'   \item{markdown}{Markdown, R Markdown, or Quarto files.}
#'   \item{workspace}{R workspace or serialized R objects.}
#'   \item{data}{Tabular spreadsheet or delimited data files.}
#'   \item{artifact}{Image artefacts.}
#'   \item{document}{PDF or word-processing documents.}
#'   \item{website_generated}{Generated website assets.}
#'   \item{other}{Files not matched by the selected profile.}
#' }
#'
#' @examples
#' toy_files <- tibble::tibble(
#'   extension = c("R", "qmd", "csv", "png", "woff2")
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
