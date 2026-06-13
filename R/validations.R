validate_journal <- function(x) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame",
      call. = FALSE
    )
  }

  required_cols <- c(
    "event_time",
    "event_type",
    "event_source",
    "event_actor",
    "resource_id",
    "resource_name",
    "observation_time"
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

  invisible(TRUE)
}
