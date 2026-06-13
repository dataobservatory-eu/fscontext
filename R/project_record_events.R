#' Project filesystem timestamps into observational events
#'
#' Creates a long-format observational event projection from a
#' filesystem snapshot or record set.
#'
#' The function unfolds filesystem timestamp metadata
#' (e.g. birth_time, ctime, mtime, atime)
#' into timestamped observational event assertions.
#'
#' This function remains observational and does not infer
#' user intent, activities, workflows, or semantic provenance.
#'
#' @param x A snapshot-like data frame.
#'
#' @param time_cols Character vector of timestamp columns
#' to project into events.
#'
#' @param id_cols Character vector of identifying columns
#' preserved during projection.
#'
#' @return
#' A tibble containing projected observational events in
#' long format.
#'
#' @importFrom dplyr any_of arrange distinct filter group_by mutate select summarise
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year month day
#' @importFrom stats setNames
#'
#' @export
project_record_events <- function(
  x,
  time_cols = c(
    "birth_time",
    "ctime",
    "mtime",
    "atime"
  ),
  id_cols = c(
    "storage_id",
    "full_path",
    "filename",
    "extension",
    "size",
    "quick_sig"
  )
) {
  if (!inherits(x, "data.frame")) {
    stop(
      "x must inherit from data.frame",
      call. = FALSE
    )
  }

  required_cols <- c(
    id_cols,
    time_cols
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  observation_time <- Sys.time()

  x %>%
    dplyr::select(
      dplyr::any_of(
        c(
          id_cols,
          time_cols
        )
      )
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      cols =
        dplyr::any_of(time_cols),
      names_to =
        "time_type",
      values_to =
        "event_time"
    ) %>%
    dplyr::filter(
      !is.na(event_time)
    ) %>%
    dplyr::mutate(
      event_date =
        as.Date(event_time),
      year =
        lubridate::year(event_time),
      month =
        lubridate::month(event_time),
      day =
        lubridate::day(event_time),
      event_type =
        .data$time_type,
      event_source =
        "filesystem",
      event_evidence =
        .data$time_type,
      event_actor =
        "unknown_local_user",
      resource_name =
        .data$filename,
      resource_id =
        .data$full_path,
      observation_time =
        observation_time
    ) %>%
    dplyr::arrange(
      .data$event_time
    )
}
