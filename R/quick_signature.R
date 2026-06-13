#' Compute a fast content signature for a file
#'
#' Generates a lightweight content signature based on hashing selected
#' byte regions of a file. This provides a fast approximation for detecting
#' identical or differing file instances without computing a full file hash.
#'
#' The function is designed for performance and is suitable for use in
#' large-scale filesystem observations, where full hashing would be
#' computationally expensive.
#'
#' @param path Character. Path to the file.
#' @param n Integer. Number of bytes to read from selected regions
#'   (default: 1024).
#'
#' @return Character. A signature string representing sampled file content.
#'
#' @details
#' The signature is constructed from hashed byte segments:
#'
#' - small files: hash of full content
#' - medium files: hash of first and last segments
#' - large files: hash of first, middle, and last segments
#'
#' The function provides a fast operational signal for probable
#' content equivalence:
#'
#' - identical signatures strongly suggest identical content
#' - different signatures indicate content differences
#' - collisions are possible but unlikely in practice
#'
#' Missing or inaccessible files return `NA_character_`.
#'
#' In RiC-aligned operational terms, the signature supports later
#' interpretation of observed filesystem Instantiations:
#'
#' - identifying likely identical Instantiations
#' - distinguishing likely versions or derivations
#' - detecting distributed or duplicated work
#' - supporting later Record Set construction and reconciliation
#'
#' The function does not establish authoritative identity or provenance.
#' It provides observational evidence that may later support analytical
#' or curatorial interpretation.
#'
#' This function is typically used in conjunction with:
#'
#' - [scan_storage()] for generating observational snapshots
#' - [summarise_duplicates()] for detecting duplicate and versioned files
#'
#' @seealso [summarise_duplicates()]
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
