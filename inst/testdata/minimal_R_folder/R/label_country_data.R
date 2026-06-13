#' Add semantic labels to a data frame column
#'
#' A minimal example wrapper around `dataset::prelabel()`
#' used in `fscontextdemo` to demonstrate semantic enrichment
#' workflows and provenance-aware data transformations.
#'
#' @param df A data frame.
#'
#' @param prelabel_map A named character vector containing
#' label mappings.
#'
#' @return
#' A data frame with a labelled `country` variable.
#'
#' @examples
#' country_map <- c(
#'   "Andorra" = "AD",
#'   "Liechtenstein" = "LI",
#'   "San Marino" = "SM"
#' )
#'
#' label_country_data(
#'   fsdemo_country_data,
#'   prelabel_map = country_map
#' )
#'
#' @importFrom dataset prelabel
#' @export
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
