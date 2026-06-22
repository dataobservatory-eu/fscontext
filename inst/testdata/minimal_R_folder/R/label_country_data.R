#' Add semantic labels to a data frame column
#'
#' A minimal example wrapper around `dataset::prelabel()`
#' used in `fscontextdemo` to demonstrate semantic enrichment
#' workflows and provenance-aware data transformations.
#' 
#' @details
#' This function serves only testing purposes and therefore it is not
#' exported and has no examples.
#' 
#'
#' @param df A data frame.
#'
#' @param prelabel_map A named character vector containing
#' label mappings.
#'
#' @return
#' A data frame with a labelled `country` variable.
#'
#' @importFrom dataset prelabel
#' @keywords internal
label_country_data <- function(
    df,
    prelabel_map
) {

  stopifnot(is.data.frame(df))

  if (!"country" %in% names(df)) {

    stop(
      "df must contain a country column.",
      call. = FALSE
    )
  }

  df$country <-
    dataset::prelabel(
      df$country,
      labels = prelabel_map
    )

  df
}
