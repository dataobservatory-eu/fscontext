#' Return a friendly greeting
#'
#' A minimal multilingual example function used in the
#' `fscontextdemo` package to demonstrate:
#'
#' - package structure
#' - roxygen2 documentation
#' - multilingual source code
#' - testing workflows
#' - Git provenance reconstruction
#'
#' The function currently supports English (`"en"`)
#' and Hungarian (`"hu"`) greetings.
#' 
#' @details
#' This function serves only testing purposes and therefore it is not
#' exported and has no examples.
#'
#' @param language_code A character language code.
#' Defaults to `"en"`.
#'
#' @return
#' A character string containing a friendly greeting.
#'
#'
#' @keywords internal
hello_world <- function(language_code = "en") {
  hello_world_string <- ""

  if (language_code == "en") {
    hello_world_string <- "Hello world!"
  }

  if (language_code == "hu") {
    hello_world_string <- "Hell\u00f3 vil\u00e1g!"
  }

  if (hello_world_string == "") {
    stop(
      "Unrecognised language code.",
      call. = FALSE
    )
  } else {
    hello_world_string
  }
}
