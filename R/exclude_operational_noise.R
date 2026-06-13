#' Exclude operational noise from analytical workflows
#'
#' @description
#' Excludes operationally low-value system and workflow artifacts from
#' analytical reconstruction workflows while preserving the original
#' observational evidence.
#'
#' The function is designed for provenance-aware analytical pipelines
#' where certain operational artifacts may:
#'
#' - inflate duplication metrics;
#' - distort reuse analysis;
#' - obscure meaningful reconstruction patterns;
#' - or reduce review efficiency.
#'
#' Unlike destructive filtering, the function is intended to operate on
#' contextual or analytical reconstruction layers after observational
#' evidence has already been preserved.
#'
#' This distinction is important for:
#'
#' - forensic reproducibility;
#' - provenance-aware reconstruction;
#' - archival transparency;
#' - and Heritage Digital Twin workflows.
#'
#' The function supports lightweight operational noise profiles.
#'
#' Current built-in profiles include:
#'
#' - `"generic"`
#' - `"rstudio"`
#'
#' The `"generic"` profile targets common system and synchronization
#' artifacts.
#'
#' The `"rstudio"` profile targets operational artifacts commonly
#' produced during R and RStudio workflows.
#'
#' Future versions may support:
#'
#' - workflow-specific profiles;
#' - institution-specific registries;
#' - YAML-based noise vocabularies;
#' - preservation-oriented filtering policies.
#'
#' The function is designed to work together with:
#'
#' - [read_snapshot()]
#' - [snapshot_to_reconstruction_context()]
#'
#' as part of layered provenance-aware reconstruction workflows.
#'
#' @param x A `data.frame` or tibble containing contextual or
#'   analytical reconstruction entities.
#'
#' @param filename Character scalar identifying the filename column.
#'
#' Defaults to `"filename"`.
#'
#' @param extension Character scalar identifying the extension column.
#'
#' Defaults to `"extension"`.
#'
#' @param profiles Character vector defining operational noise
#'   profiles to apply.
#'
#' Current profiles include:
#'
#' - `"generic"`
#' - `"rstudio"`
#'
#' @return
#' A filtered `data.frame` excluding operational noise resources.
#'
#' @details
#' The function intentionally excludes only operationally low-priority
#' resources.
#'
#' It does not:
#'
#' - delete observational evidence;
#' - modify the original snapshot data;
#' - infer preservation value;
#' - determine archival significance;
#' - replace curatorial review.
#'
#' Resources excluded from analytical workflows may still remain
#' important for:
#'
#' - forensic preservation;
#' - synchronization reconstruction;
#' - reproducibility auditing;
#' - or operational environment analysis.
#'
#' @examples
#' toy_files <- tibble::tibble(
#'   filename = c(
#'     ".DS_Store",
#'     ".Rhistory",
#'     "analysis.R",
#'     "report.qmd"
#'   ),
#'   extension = c(
#'     "",
#'     "",
#'     "R",
#'     "qmd"
#'   )
#' )
#'
#' exclude_operational_noise(
#'   toy_files,
#'   profiles = c(
#'     "generic",
#'     "rstudio"
#'   )
#' )
#'
#' @importFrom stats setNames
#' @export


exclude_operational_noise <- function(
  x,
  filename = "filename",
  extension = "extension",
  profiles = c(
    "generic",
    "rstudio"
  )
) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame or tibble",
      call. = FALSE
    )
  }

  if (!filename %in% names(x)) {
    stop(
      "Column not found: ",
      filename,
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

  # --------------------------------------------------------------
  # PROFILE REGISTRIES
  # --------------------------------------------------------------

  ignored_names <- character()

  ignored_extensions <- character()

  # --------------------------------------------------------------
  # GENERIC OPERATIONAL NOISE
  # --------------------------------------------------------------

  if ("generic" %in% profiles) {
    ignored_names <- c(
      ignored_names,
      ".DS_Store",
      "Thumbs.db",
      "dir.c9r",
      "masterkey.cryptomator",
      "vault.cryptomator"
    )
  }

  # --------------------------------------------------------------
  # R / RSTUDIO WORKFLOW NOISE
  # --------------------------------------------------------------

  if ("rstudio" %in% profiles) {
    ignored_names <- c(
      ignored_names,
      ".Rhistory",
      ".RData",
      ".Rproj.user",
      ".gitignore"
    )
  }

  ext <- tolower(
    x[[extension]]
  )

  is_noise_name <-
    x[[filename]] %in%
    unique(ignored_names)

  is_noise_extension <-
    ext %in%
    unique(ignored_extensions)

  x[!(
    is_noise_name |
      is_noise_extension
  ), ]
}
