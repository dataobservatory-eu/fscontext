## code to prepare `fsdemo_country_data` dataset goes here

set.seed(2026)

country_names <- c(
  "Andorra",
  "Liechtenstein",
  "San Marino"
)

country_codes <- c(
  "AD",
  "LI",
  "SM"
)

fsdemo_country_data <- tibble::tibble(
  country = sample(
    c(country_names, country_codes),
    size = 5,
    replace = TRUE
  ),
  time = sample(
    c(2024, 2025),
    size = 5,
    replace = TRUE
  ),
  value = round(
    stats::runif(
      n = 5,
      min = 1,
      max = 5
    ),
    digits = 1
  )
)

usethis::use_data(
  fsdemo_country_data,
  overwrite = TRUE
)
