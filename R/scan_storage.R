#' Observe a filesystem and construct a reproducible snapshot
#'
#' Recursively scans a root folder and returns a data.frame where each row
#' represents one filesystem observation recorded at a specific time.
#'
#' The function implements a read-only filesystem observation model:
#'
#' - it records accessible filesystem state;
#' - it does not interpret file contents;
#' - it does not assume canonical, complete, or authoritative state.
#'
#' Each observation records:
#'
#' - a relative filesystem locator (`rel_path`);
#' - a storage context (`storage_id`);
#' - an observation timestamp (`scan_time`).
#'
#' Additional metadata may include:
#'
#' - filesystem properties (size, timestamps, permissions);
#' - optional content signatures (`quick_sig`);
#' - repository and version-control context
#'   (`repo_root`, `repo_rel_path`, `git_tracked`).
#'
#' The package deliberately records filesystem observations first and
#' postpones documentary interpretation, Record Set construction,
#' and RiC-aligned semantic assertions to later analytical stages.
#'
#' This creates a reproducible observational snapshot suitable for:
#'
#' - forensic analysis of development environments;
#' - reconstruction of activity patterns;
#' - audit and compliance workflows;
#' - alignment with version-controlled repositories.
#'
#' @param root Character. Path to the root folder to observe.
#'
#' @param storage_id Character. Identifier of the storage context.
#'
#' @param person_id Character. Identifier of the observer or operator.
#'
#' @param scan_time POSIXct. Timestamp of the observation.
#'   Defaults to `Sys.time()` if not provided.
#'
#' @param compute_signature Logical. Whether to compute lightweight
#'   content signatures.
#'
#' @param max_signature_size Numeric. Maximum file size (bytes)
#'   for signature computation.
#'
#' @return
#' A `data.frame` where each row represents one filesystem observation.
#'
#' @details
#' The returned dataset at minimum contains:
#'
#' - `rel_path`: relative filesystem locator within the observed root;
#' - `storage_path_id`: deterministic storage-scoped identifier derived
#'   from `storage_id::rel_path`;
#' - `filename`: basename of the observed file;
#' - `mtime`: last modification timestamp;
#' - `extension`: file extension.
#'
#' Additional variables may be present depending on scan configuration.
#'
#' The function is:
#'
#' - read-only and non-destructive;
#' - deterministic for a given filesystem state;
#' - robust to inaccessible files, which are silently skipped.
#'
#' The result represents observed filesystem state rather than complete
#' historical provenance.
#'
#' @seealso [snapshot_storage()]
#'
#' @importFrom utils flush.console
#'
#' @export
scan_storage <- function(root,
                         storage_id = "l480-1-ssd",
                         person_id = "antaldaniel",
                         scan_time = Sys.time(),
                         compute_signature = TRUE,
                         max_signature_size = 200 * 1024 * 1024) {
  start_time <- Sys.time()
  message("Starting scan_storage() on: ", root)

  root <- fs::path_abs(root)

  if (!fs::dir_exists(root)) {
    stop("scan_storage(): root path does not exist: ", root, call. = FALSE)
  }

  # --- list files safely ---
  files <- fs::dir_ls(
    path = root,
    recurse = TRUE,
    all = TRUE,
    type = "file",
    fail = FALSE
  )

  # --- early feedback ---
  n_files <- length(files)

  if (n_files == 0) {
    message("No files found.")
  } else if (n_files > 10000) {
    message("Scanning ", n_files, " files, this may take several minutes...")
  } else if (n_files > 1000) {
    message("Scanning ", n_files, " files, this may take 1-2 minutes...")
  } else {
    message("Scanning ", n_files, " files.")
  }

  # --- exclude noise ---
  ignore_patterns <- c(
    "[/\\\\]\\.git[/\\\\]",
    "[/\\\\]\\.Rproj\\.user[/\\\\]",
    "\\$RECYCLE\\.BIN",
    "System Volume Information"
  )

  for (pat in ignore_patterns) {
    files <- files[!grepl(pat, files)]
  }

  # --- safe file_info ---
  info <- tryCatch(
    fs::file_info(files),
    error = function(e) {
      message("file_info() failed, retrying per-file...")
      do.call(rbind, lapply(files, function(f) {
        tryCatch(fs::file_info(f), error = function(e) NULL)
      }))
    }
  )

  # ensure alignment
  valid <- !is.na(info$size)
  files <- files[valid]
  info <- info[valid, ]

  rel_path <- fs::path_rel(files, start = root)

  filename <- fs::path_file(files)
  ext <- fs::path_ext(files)

  df <- data.frame(
    storage_id = storage_id,
    person_id = person_id,
    full_path = as.character(files),
    rel_path = as.character(rel_path),
    filename = filename,
    stem = tools::file_path_sans_ext(filename),
    extension = ifelse(ext == "", NA_character_, tolower(ext)),
    type = info$type,
    size = as.double(info$size),
    mtime = info$modification_time,
    ctime = info$change_time,
    atime = info$access_time,
    birth_time = info$birth_time,
    depth = lengths(strsplit(rel_path, "[/\\\\]+")),
    links = info$hard_links,
    permissions = info$permissions,
    quick_sig = NA_character_,
    scan_time = scan_time,
    stringsAsFactors = FALSE
  )

  # --- ALWAYS define columns (test contract) ---
  df$repo_root <- NA_character_
  df$repo_rel_path <- NA_character_
  df$git_tracked <- NA

  # --- signatures ---
  if (compute_signature && nrow(df) > 0) {
    eligible_idx <- which(df$size <= max_signature_size)

    if (length(eligible_idx) > 0) {
      pb <- progress::progress_bar$new(
        format = "  computing signatures [:bar] :percent (:current/:total)",
        total = length(eligible_idx),
        clear = FALSE,
        width = 60,
        stream = stderr()
      )

      safe_quick_signature <- function(p) {
        pb$tick()
        utils::flush.console()
        tryCatch(quick_signature(p), error = function(e) NA_character_)
      }

      df$quick_sig[eligible_idx] <- vapply(
        df$full_path[eligible_idx],
        safe_quick_signature,
        character(1)
      )
    }
  }

  message("Signatures computed. Detecting repositories and Git status...")


  # --- storage-scoped logical file identity ---
  # storage_path_id approximates a stable Record Resource identity within
  # one storage context. Observation-specific identity is added later
  # via add_snapshot_context().

  df$storage_path_id <- paste(df$storage_id, df$rel_path, sep = "::")

  # --- detect repo roots safely ---
  git_dirs <- tryCatch(
    fs::dir_ls(
      root,
      recurse = TRUE,
      type = "directory",
      all = TRUE,
      regexp = "[/\\\\]\\.git$",
      fail = FALSE
    ),
    error = function(e) character()
  )

  repo_roots <- unique(dirname(git_dirs))
  repo_roots_norm <- fs::path_abs(repo_roots)

  find_repo_root_safe <- function(path) {
    matches <- repo_roots_norm[startsWith(path, repo_roots_norm)]
    if (length(matches) == 0) {
      return(NA_character_)
    }
    matches[which.max(nchar(matches))]
  }

  if (length(repo_roots_norm) > 0) {
    df$repo_root <- vapply(
      df$full_path,
      find_repo_root_safe,
      character(1)
    )

    # --- repo_rel_path ---
    df$repo_rel_path <- vapply(
      seq_len(nrow(df)),
      function(i) {
        if (is.na(df$repo_root[i])) {
          return(NA_character_)
        }
        fs::path_rel(df$full_path[i], start = df$repo_root[i])
      },
      character(1)
    )

    # --- git tracked ---
    message("Checking Git tracked files...")

    for (repo in repo_roots_norm) {
      idx <- which(!is.na(df$repo_root) & df$repo_root == repo)
      if (length(idx) == 0) next

      tracked <- tryCatch(
        system2("git",
          args = c("-C", repo, "ls-files"),
          stdout = TRUE,
          stderr = TRUE
        ),
        warning = function(w) character(),
        error = function(e) character()
      )

      if (length(tracked) == 0) next

      df$git_tracked[idx] <- df$repo_rel_path[idx] %in% tracked
    }
  }

  # --- repo metadata ---
  if (length(repo_roots_norm) > 0) {
    repos_df <- data.frame(
      repo_root = repo_roots_norm,
      git_remote = vapply(repo_roots_norm, get_git_remote, character(1)),
      git_branch = vapply(repo_roots_norm, get_git_branch, character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    repos_df <- data.frame(
      repo_root = character(),
      git_remote = character(),
      git_branch = character(),
      stringsAsFactors = FALSE
    )
  }

  # --- attributes ---
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("fscontext")),
    error = function(e) NA_character_
  )

  attr(df, "created_at") <- scan_time
  attr(df, "created_by") <- "scan_storage"
  attr(df, "package") <- "fscontext"
  attr(df, "package_version") <- pkg_version
  attr(df, "repos") <- repos_df
  attr(df, "schema_version") <- "0.1.3"
  attr(df, "scan_root") <- root

  attr(df, "scan_call") <- paste(
    deparse(match.call()),
    collapse = " "
  )

  attr(df, "signature_enabled") <- compute_signature
  attr(df, "max_signature_size") <- max_signature_size

  end_time <- Sys.time()
  elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
  skipped_estimate <- n_files - nrow(df)

  message("Files scanned: ", nrow(df))
  message("Files in Git repos: ", sum(!is.na(df$repo_root)))
  message("Files tracked by Git: ", sum(df$git_tracked, na.rm = TRUE))
  message("Skipped approximately ", skipped_estimate, " inaccessible files")
  message("scan_storage completed in ", elapsed, " seconds")

  df
}
