#' Compute a fast operational signature for a file
#'
#' Generates a lightweight content signature by hashing sampled byte
#' regions from a file. The signature provides a fast operational
#' approximation for detecting identical or differing file instances
#' without computing a full cryptographic hash.
#'
#' The function is designed for large-scale observational workflows
#' where complete file hashing would be unnecessarily expensive.
#'
#' @param path Character. Path to the file.
#' @param n Integer. Number of bytes sampled from selected regions
#'   (default: 1024).
#'
#' @return Character. A lightweight operational signature.
#'
#' @details
#' The signature is constructed by hashing sampled byte regions:
#'
#' * small files: full file content
#' * medium files: beginning and end
#' * large files: beginning, middle and end
#'
#' The resulting signature is intended as a fast observational aid:
#'
#' * identical signatures suggest identical file content;
#' * differing signatures indicate differing file content;
#' * collisions are possible but unlikely in operational use.
#'
#' Missing or inaccessible files return `NA_character_`.
#'
#' The signature does not establish authoritative identity or provenance.
#' It provides lightweight observational evidence that may support later
#' contextual reconstruction, duplicate detection, version analysis,
#' or Record Set construction.
#'
#' Unlike [quick_signature_text()], this function operates on the
#' binary representation of a file rather than its textual content.
#'
#' @seealso
#' [quick_signature_text()],
#' [scan_storage()],
#' [summarise_duplicates()]
#'
#' @importFrom fs file_info
#' @importFrom digest digest
#' @export

quick_signature <- function(path, n = 1024) {
  info <- fs::file_info(path)
  size <- info$size

  if (is.null(size) || length(size) == 0 || is.na(size)) {
    return(NA_character_)
  }

  if (size == 0) {
    return("empty")
  }

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  # --- case 1: very small ---
  # full-content approximation is inexpensive

  if (size <= n) {
    bytes <- readBin(con, "raw", n = size)

    return(digest::digest(bytes, algo = "xxhash32"))
  }

  # --- first segment ---
  # stable operational signal for fast comparison

  first <- readBin(con, "raw", n = n)

  # --- case 2: medium ---
  # compare beginning and end of file

  if (size <= 3 * n) {
    seek(con, where = size - n, origin = "start")

    last <- readBin(con, "raw", n = n)

    return(paste0(
      digest::digest(first, algo = "xxhash32"),
      "_",
      digest::digest(last, algo = "xxhash32")
    ))
  }

  # --- case 3: large ---
  # compare beginning, middle, and end regions
  # suitable for large-scale filesystem observation

  seek(con, where = floor(size / 2), origin = "start")

  middle <- readBin(con, "raw", n = n)

  seek(con, where = size - n, origin = "start")

  last <- readBin(con, "raw", n = n)

  paste0(
    digest::digest(first, algo = "xxhash32"), "_",
    digest::digest(middle, algo = "xxhash32"), "_",
    digest::digest(last, algo = "xxhash32")
  )
}


#' Compute a fast operational signature for text
#'
#' Generates a lightweight content signature by hashing sampled character
#' regions from one or more text strings. The signature provides a fast
#' approximation for detecting identical or differing textual content
#' without comparing complete strings.
#'
#' The function is intended for observational workflows where textual
#' representations have already been extracted from digital resources,
#' such as HTML pages, OCR output, PDFs, or office documents.
#'
#' @param x Character vector.
#' @param n Integer. Number of characters sampled from selected regions
#'   (default: 1024).
#'
#' @return Character vector of operational signatures that summarises the
#' observed textual representation of a resource.
#'
#' @details
#' The signature is constructed by hashing sampled character regions:
#'
#' * short texts: complete text;
#' * medium texts: beginning and end;
#' * long texts: beginning, middle and end.
#'
#' The resulting signature is intended as a fast observational aid:
#'
#' * identical signatures suggest identical textual content;
#' * differing signatures indicate differing textual content;
#' * collisions are possible but unlikely in operational use.
#'
#' Missing values return `NA_character_`.
#' Empty strings return `"empty"`.
#'
#' Unlike [quick_signature()], this function operates on extracted text
#' rather than binary file content. Consequently, different file formats
#' (for example DOCX, PDF and HTML) containing the same textual content
#' may produce identical text signatures while retaining different file
#' signatures.
#'
#' The function provides lightweight observational evidence that may
#' support duplicate detection, content reconciliation, semantic
#' stabilisation, or later contextual reconstruction.
#'
#' @seealso
#' [quick_signature()],
#' [observe_wacz()]
#'
#' @importFrom digest digest
#' @export
quick_signature_text <- function(x, n = 1024) {
  if (length(x) == 0) {
    return(character())
  }

  vapply(
    x,
    function(text) {
      if (is.na(text)) {
        return(NA_character_)
      }

      text <- enc2utf8(text)

      if (!nzchar(text)) {
        return("empty")
      }

      n_chars <- nchar(text, type = "chars")

      if (n_chars <= n) {
        return(digest::digest(text, algo = "xxhash32"))
      }

      first <- substr(text, 1, n)

      if (n_chars <= 3 * n) {
        last <- substr(text, n_chars - n + 1, n_chars)

        return(paste0(
          digest::digest(first, algo = "xxhash32"),
          "_",
          digest::digest(last, algo = "xxhash32")
        ))
      }

      middle_start <- floor(n_chars / 2)
      middle <- substr(text, middle_start, middle_start + n - 1)
      last <- substr(text, n_chars - n + 1, n_chars)

      paste0(
        digest::digest(first, algo = "xxhash32"), "_",
        digest::digest(middle, algo = "xxhash32"), "_",
        digest::digest(last, algo = "xxhash32")
      )
    },
    character(1)
  )
}
