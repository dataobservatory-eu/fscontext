#' Subset observational filesystem Instantiations
#'
#' Selects observations from a snapshot according to structural
#' criteria such as paths, extensions, or exclusion patterns.
#'
#' The function performs observational selection only and does not
#' derive Record Sets, contextual hierarchies, or analytical groupings.
#'
#' @details
#' The function returns a subset of the original snapshot while preserving
#' its observational provenance (e.g. `created_by`, `created_at`).
#'
#' In addition, it derives a `rel_root_path` column, which represents the
#' path of each file relative to the matched filter root. When multiple
#' `folder_path` values are provided, the deepest matching root is used.
#'
#' The `rel_root_path` is a context-dependent projection intended for
#' grouping, navigation, and reporting. It is not a stable identifier and
#' should not be used for joins or identity; use `rel_path` for that purpose.
#'
#' @param snapshot_path Character. Path to `.rds` snapshot file.
#' @param folder_path Character vector. One or more folder roots.
#' @param extensions Optional character vector of file extensions (no dot).
#' @param exclude_patterns Optional regex patterns to exclude paths.
#'
#' @return data.frame filtered snapshot with `rel_root_path`
#' @examples
#' data("fscontextdemo_snapshot_02")
#'
#' tmp <- tempfile(fileext = ".rds")
#' saveRDS(fscontextdemo_snapshot_02, tmp)
#'
#' subset_snapshot(
#'   snapshot_path = tmp,
#'   folder_path = "D:/_packages/fscontextdemo/R"
#' )
#' @export
subset_snapshot <- function(snapshot_path,
                            folder_path,
                            extensions = NULL,
                            exclude_patterns = c("\\.Rcheck")) {
  df <- readRDS(snapshot_path)

  orig_created_by <- attr(df, "created_by")
  orig_created_at <- attr(df, "created_at")


  folder_path <- normalize_context_roots(folder_path) # see utils.R
  full_path_norm <- normalize_context_roots(df$full_path)

  # --- folder filtering ---
  keep <- Reduce(`|`, lapply(folder_path, function(root) {
    full_path_norm == root | startsWith(full_path_norm, paste0(root, "/"))
  }))

  df <- df[keep, , drop = FALSE]
  full_path_norm <- full_path_norm[keep]

  if (nrow(df) == 0) {
    return(df)
  }

  # --- exclude patterns ---
  if (!is.null(exclude_patterns)) {
    drop <- Reduce(`|`, lapply(exclude_patterns, function(pat) {
      grepl(pat, df$full_path)
    }))

    df <- df[!drop, , drop = FALSE]
    full_path_norm <- full_path_norm[!drop]
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # --- extension filter ---
  if (!is.null(extensions)) {
    extensions <- tolower(gsub("^\\.", "", extensions))

    keep_ext <- !is.na(df$extension) &
      tolower(df$extension) %in% extensions

    df <- df[keep_ext, , drop = FALSE]
    full_path_norm <- full_path_norm[keep_ext]
  }

  if (nrow(df) == 0) {
    return(df)
  }

  # --- escape regex safely ---
  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  }

  # --- compute relative path (deepest match) ---
  df$rel_root_path <- vapply(seq_len(nrow(df)), function(i) {
    roots <- folder_path[
      full_path_norm[i] == folder_path |
        startsWith(full_path_norm[i], paste0(folder_path, "/"))
    ]

    root <- roots[which.max(nchar(roots))]

    sub(paste0("^", escape_regex(root), "/?"), "", full_path_norm[i])
  }, character(1))

  # restore original provenance
  attr(df, "created_by") <- orig_created_by
  attr(df, "created_at") <- orig_created_at

  # add derived metadata
  attr(df, "derived_by") <- "subset_snapshot"
  attr(df, "derived_at") <- Sys.time()
  attr(df, "schema_version") <- "0.1.2"
  attr(df, "package_version") <- "0.1.2"
  attr(df, "source_snapshot") <- snapshot_path
  attr(df, "filter_params") <- list(
    folder_path = folder_path,
    extensions = extensions,
    exclude_patterns = exclude_patterns
  )

  df
}
