#' Extract a stable path prefix at a fixed structural depth
#'
#' Returns the first `depth` components of each path as a normalized
#' forward-slash-separated prefix. This provides a deterministic and
#' reproducible way to derive a *structural grouping key* from file paths.
#'
#' The function does not interpret paths (e.g. no special handling of
#' filenames, repositories, or working directories). It simply truncates
#' the path at the requested depth after normalizing separators.
#'
#' Typical use cases include:
#' - grouping files by project or subproject (e.g. depth = 2)
#' - creating stable aggregation units for audit, reporting, or timesheets
#' - deriving Record Set–like groupings in RiC-aligned archival workflows
#'
#' behaviour is fully deterministic:
#' - identical inputs always produce identical outputs
#' - paths shorter than `depth` are returned unchanged
#' - `depth = 0` returns an empty string (explicit root-level grouping)
#'
#' @param path Character vector of file paths (relative or absolute)
#' @param depth Integer. Number of leading path components to keep (>= 0)
#'
#' @return Character vector of normalized path prefixes
#'
#' @examples
#' path_prefix("_eviota/reporting/R/scan_storage.R", depth = 2)
#' # "_eviota/reporting"
#'
#' path_prefix(c("a/b/c.txt", "x/y"), depth = 1)
#' # c("a", "x")
#'
#' @export
path_prefix <- function(path, depth = 2) {
  stopifnot(is.character(path))

  depth <- as.integer(depth[1])

  if (is.na(depth) || depth < 0) {
    stop("depth must be a non-negative integer", call. = FALSE)
  }

  # normalize separators
  path <- gsub("\\\\", "/", path)

  parts <- strsplit(path, "/+", perl = TRUE)

  vapply(parts, function(p) {
    if (length(p) == 0) {
      return("")
    }

    if (depth == 0) {
      return("")
    } # explicit choice

    n <- min(length(p), depth)
    paste(p[seq_len(n)], collapse = "/")
  }, character(1))
}
