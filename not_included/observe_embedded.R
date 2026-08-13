files <- c("F:\028745_ERM_Fk213_170_028745-retouched2.jpg")

dir ("F:")

library(fs)
files <- dir("F:/LOAM", recursive = T)
files <- file.path("F:/LOAM", files)

exiftool_json <- function(files) {
  
  out <- system2(
    "exiftool",
    c("-j", files),
    stdout = TRUE
  )
  
  jsonlite::fromJSON(
    paste(out, collapse = "\n"),
    simplifyDataFrame = TRUE
  )
  
}

extensions = c(
  # Images
  "jpg", "jpeg", "png", "tif", "tiff",
  "gif", "bmp", "webp", "heic", "heif",
  
  # Camera RAW
  "orf", "cr2", "cr3", "nef", "nrw",
  "arw", "sr2", "raf", "rw2", "dng",
  "pef", "3fr", "iiq", "x3f",
  
  # Video
  "mp4", "mov", "avi", "mkv",
  "mts", "m2ts", "mpeg", "mpg",
  
  # Audio
  "wav", "bwf", "mp3", "aac",
  "flac", "ogg", "aif", "aiff",
  "m4a", "wma"
)


observe_embedded <- function(
    files,
    engine = "exiftool",
    extensions = NULL,
    ignore_errors = TRUE,
    progress = interactive(),
    ...
) {
  
  engine <- match.arg(engine, "exiftool")
  
  stopifnot(is.character(files))
  
  ## Filter by extension if requested
  if (!is.null(extensions)) {
    ext <- tolower(fs::path_ext(files))
    files <- files[ext %in% tolower(extensions)]
  }
  
  if (length(files) == 0)
    return(tibble::tibble())
  
  message("Observing ", length(files), " files.")
  
  ## ---- Observation activity ---------------------------------------------
  
  activity_id <- paste0(
    "observe_embedded_",
    format(Sys.time(), "%Y%m%d%H%M%OS3")
  )
  
  observation_started <- Sys.time()
  
  engine_version <- tryCatch(
    system2(engine, "-ver", stdout = TRUE),
    error = function(e) NA_character_
  )
  
  ## ---- Progress bar ------------------------------------------------------
  
  if (progress) {
    pb <- utils::txtProgressBar(
      min = 0,
      max = length(files),
      style = 3
    )
    on.exit(close(pb), add = TRUE)
  }
  
  ## ---- Observe files -----------------------------------------------------
  
  metadata <- vector("list", length(files))
  observed <- logical(length(files))
  error <- rep(NA_character_, length(files))
  
  for (i in seq_along(files)) {
    
    res <- tryCatch({
      
      out <- suppressWarnings(
        system2(
          engine,
          c("-j", files[[i]]),
          stdout = TRUE,
          stderr = TRUE
        )
      )
      
      x <- jsonlite::fromJSON(
        paste(out, collapse = "\n"),
        simplifyDataFrame = TRUE
      )
      
      exif_error <- NA_character_
      
      if ("Error" %in% names(x)) {
        exif_error <- x$Error
        x$Error <- NULL
      }
      
      list(
        observed = TRUE,
        metadata = x,
        error = exif_error
      )
      
    }, error = function(e) {
      
      list(
        observed = FALSE,
        metadata = tibble::tibble(),
        error = conditionMessage(e)
      )
      
    })
    
    observed[i] <- res$observed
    metadata[[i]] <- res$metadata
    error[i] <- res$error
    
    if (progress)
      utils::setTxtProgressBar(pb, i)
  }
  
  ## ---- Finish ------------------------------------------------------------
  
  observation_finished <- Sys.time()
  
  elapsed_seconds <- as.numeric(
    difftime(
      observation_finished,
      observation_started,
      units = "secs"
    )
  )
  
  average_seconds <- round(elapsed_seconds / length(files), 2)
  
  message(round(elapsed_seconds, 2), " seconds")
  message("Average time per file: ", average_seconds, " seconds")
  
  embedded_df <- tibble::tibble(
    file = files,
    observed = observed,
    metadata = metadata,
    error = error
  )
  
  attr(embedded_df, "observation") <- list(
    activity_id = activity_id,
    observation_started = observation_started,
    observation_finished = observation_finished,
    observer = engine,
    observer_version = engine_version,
    n_files = length(files),
    elapsed_seconds = elapsed_seconds,
    average_seconds = average_seconds
  )
  
  embedded_df
}

inventory_embedded_fields <- function(x) {
  
  stopifnot("metadata" %in% names(x))
  
  out <- vector("list", nrow(x))
  
  for (i in seq_len(nrow(x))) {
    
    md <- x$metadata[[i]]
    
    out[[i]] <- data.frame(
      file = x$file[i],
      extension = tolower(fs::path_ext(x$file[i])),
      field = names(md),
      stringsAsFactors = FALSE
    )
    
  }
  
  do.call(rbind, out)
  
}

observe_embedded(files[1:5])

repo_rel <- file.path("D:/_assets/test", dir("D:/_assets/test", recursive = T))

res <- observe_embedded(repo_rel)
res

test <- inventory_embedded_fields(res)
library(dplyr)
test %>% distinct ( field, extension) %>%  filter (grepl("date", tolower(field)))
