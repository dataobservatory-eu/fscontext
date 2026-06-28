#' Observe a WACZ web archive
#'
#' @description
#' Creates an observational data frame from a WACZ web archive.
#'
#' The function extracts structural metadata from the archive,
#' combines page-level information with WARC index metadata, and returns
#' one observational row for each archived web page.
#'
#' The resulting object represents observations only. It intentionally
#' avoids making semantic assertions about Records, Record Parts,
#' Instantiations, or other archival entities. Such interpretation can
#' be added later with [wacz_to_recordset_df()] or downstream semantic
#' enrichment workflows.
#'
#' @param wacz
#' Path to a `.wacz` archive.
#'
#' @return
#' A tibble containing observations extracted from the archive.
#'
#' The returned object carries two attributes:
#'
#' * `datapackage`, containing the parsed `datapackage.json`
#'   metadata supplied by the WACZ archive;
#' * `wacz`, containing the normalized path to the source archive.
#'
#' Typical variables include:
#'
#' * page identifiers;
#' * resource locators (URLs);
#' * page titles;
#' * timestamps;
#' * extracted text;
#' * text signatures;
#' * MIME types;
#' * WARC digests;
#' * archive offsets;
#' * version counts.
#'
#' @details
#' The function performs the following steps:
#'
#' * extracts the WACZ archive into a temporary directory;
#' * reads the archive `datapackage.json`;
#' * parses page metadata from `pages/pages.jsonl`;
#' * parses WARC index metadata from `indexes/index.cdx`;
#' * collapses multiple archived versions of the same resource;
#' * joins page observations with archive metadata.
#'
#' The resulting observations preserve the evidence contained in the
#' archive without interpreting its archival semantics.
#'
#' @references
#' The WACZ format specification:
#' \url{https://specs.webrecorder.net/wacz/}
#'
#' @seealso
#' [wacz_to_recordset_df()]
#'
#' @examples
#' wacz <- system.file("testdata", "fscontext_020.wacz", package = "fscontext")
#'
#' observe_wacz(wacz)
#'
#' @export

observe_wacz <- function(wacz) {
  tmp <- tempfile("wacz")

  extract_storage(
    archive = wacz,
    exdir = tmp
  )

  datapackage <- read_datapackage(tmp)

  pages <- read_pages_jsonl(tmp)

  cdx <- read_cdx(tmp) |>
    collapse_cdx_versions()

  observations <- match_pages_to_cdx(
    pages,
    cdx
  ) |>
    dplyr::mutate(
      archive = basename(wacz),
      full_path = normalizePath(wacz)
    )

  attr(observations, "datapackage") <- datapackage
  attr(observations, "wacz") <- normalizePath(wacz)

  observations
}

#' @keywords internal
#' @importFrom jsonlite stream_in
#' @importFrom dplyr filter mutate rename
#' @importFrom tibble as_tibble
#' @noRd

read_pages_jsonl <- function(path) {
  pages_file <- file.path(path, "pages", "pages.jsonl")

  if (!file.exists(pages_file)) {
    stop(
      "Cannot find 'pages/pages.jsonl' in ",
      path,
      call. = FALSE
    )
  }

  pages <- suppressWarnings(
    jsonlite::stream_in(
      file(pages_file),
      verbose = FALSE
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      page_id = id,
      resource_locator = url,
      timestamp = ts,
      has_text = hasText,
      favicon = favIconUrl
    ) |>
    dplyr::filter(!is.na(resource_locator)) |>
    dplyr::mutate(
      text_length = nchar(text),
      quick_sig_text = quick_signature_text(text)
    )
}

#' @keywords internal
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr mutate rename
#' @importFrom purrr map_dfr
#' @noRd

read_cdx <- function(path) {
  cdx_file <- file.path(path, "indexes", "index.cdx")

  if (!file.exists(cdx_file)) {
    stop(
      "Cannot find 'indexes/index.cdx' in ",
      path,
      call. = FALSE
    )
  }

  lines <- readLines(
    cdx_file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  lines <- lines[nzchar(lines)]

  parsed <- purrr::map_dfr(
    lines,
    function(line) {
      parts <- strsplit(
        line,
        " ",
        fixed = TRUE
      )[[1]]

      if (length(parts) < 3) {
        return(NULL)
      }

      urlkey <- parts[1]
      timestamp <- parts[2]
      json <- paste(
        parts[-c(1, 2)],
        collapse = " "
      )

      meta <- jsonlite::fromJSON(json)

      tibble::tibble(
        urlkey = urlkey,
        cdx_timestamp = timestamp,
        resource_locator = meta$url,
        digest = meta$digest %||% NA_character_,
        mime = meta$mime %||% NA_character_,
        offset = meta$offset %||% NA_real_,
        length = meta$length %||% NA_real_,
        record_digest = meta$recordDigest %||% NA_character_,
        status = meta$status %||% NA_integer_,
        warc_filename = meta$filename %||% NA_character_
      )
    }
  )

  parsed
}

#' Collapse repeated WACZ observations of the same archived resource
#'
#' @keywords internal
#' @noRd

collapse_cdx_versions <- function(cdx) {
  version_count <-
    cdx |>
    dplyr::count(
      resource_locator,
      name = "n_versions"
    )

  cdx |>
    dplyr::filter(
      mime == "text/html"
    ) |>
    dplyr::group_by(resource_locator) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      version_count,
      by = "resource_locator"
    )
}


#' Match page observations to archived payload metadata
#'
#' @keywords internal
#' @noRd

match_pages_to_cdx <- function(
  pages,
  cdx
) {
  dplyr::left_join(
    pages,
    cdx,
    by = "resource_locator"
  )
}

#' @keywords internal
#' @importFrom jsonlite read_json
#' @importFrom tibble as_tibble
#' @noRd

read_datapackage <- function(path) {
  datapackage_file <- file.path(path, "datapackage.json")

  if (!file.exists(datapackage_file)) {
    stop(
      "Cannot find 'datapackage.json' in ",
      path,
      call. = FALSE
    )
  }

  dp <- jsonlite::read_json(
    datapackage_file,
    simplifyVector = TRUE
  )

  dp
}
