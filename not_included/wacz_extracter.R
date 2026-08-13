# -------------------------------------------------------------------------
# WACZ curatorial ingestion
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Read one WARC record
# -------------------------------------------------------------------------

# Read one compressed WARC record using the byte range supplied by
# index.cdx and return the decompressed record as raw bytes.

read_wacz_record <- function(
    root,
    warc_filename,
    offset,
    length
) {
  
  warc_gz <- file.path(
    root,
    "archive",
    warc_filename
  )
  
  con <- file(warc_gz, "rb")
  on.exit(close(con))
  
  seek(
    con,
    where = offset,
    origin = "start"
  )
  
  compressed_record <- readBin(
    con,
    what = "raw",
    n = length
  )
  
  record_gz <- tempfile(fileext = ".warc.gz")
  writeBin(compressed_record, record_gz)
  
  gz_con <- gzfile(record_gz, "rb")
  on.exit(close(gz_con), add = TRUE)
  
  readBin(
    gz_con,
    what = "raw",
    n = 100 * 1024 * 1024
  )
}


# -------------------------------------------------------------------------
# Extract HTML from a WARC response
# -------------------------------------------------------------------------

extract_html_document <- function(record) {
  
  record <- rawToChar(record)
  
  start <- regexpr(
    "<!DOCTYPE",
    record,
    fixed = TRUE
  )
  
  if (start[1] == -1) {
    start <- regexpr(
      "<html",
      record,
      fixed = TRUE
    )
  }
  
  if (start[1] == -1) {
    return(NA_character_)
  }
  
  substr(
    record,
    start[1],
    nchar(record)
  )
}


# -------------------------------------------------------------------------
# Source-specific representative image extraction
# -------------------------------------------------------------------------

# MUIS explicitly marks the representative image as name="thumbnail".

extract_muis_thumbnail <- function(html) {
  
  html |>
    xml2::read_html() |>
    xml2::xml_find_first(
      ".//img[@name='thumbnail']"
    ) |>
    xml2::xml_attr("src")
}


# Finna exposes the representative image through Open Graph metadata.
# Use the small rendition for the review interface.

extract_finna_thumbnail <- function(html) {
  
  image_url <- html |>
    xml2::read_html() |>
    xml2::xml_find_first(
      ".//meta[@property='og:image']"
    ) |>
    xml2::xml_attr("content")
  
  sub(
    "size=large",
    "size=small",
    image_url,
    fixed = TRUE
  )
}


# Garamantas marks the representative photograph with class="image-zoom".

extract_garamantas_thumbnail <- function(html) {
  
  html |>
    xml2::read_html() |>
    xml2::xml_find_first(
      ".//img[contains(concat(' ', normalize-space(@class), ' '), ' image-zoom ')]"
    ) |>
    xml2::xml_attr("src")
}


# -------------------------------------------------------------------------
# Dispatch representative image extraction by source
# -------------------------------------------------------------------------

extract_thumbnail_url <- function(
    html,
    source
) {
  
  if (is.na(html)) {
    return(NA_character_)
  }
  
  switch(
    source,
    muis = extract_muis_thumbnail(html),
    finna = extract_finna_thumbnail(html),
    garamantas = extract_garamantas_thumbnail(html),
    NA_character_
  )
}


# -------------------------------------------------------------------------
# Prepare a WACZ for curatorial review
# -------------------------------------------------------------------------

prepare_wacz_review_pages <- function(wacz) {
  
  # Extract the WACZ package.
  tmp <- tempfile("wacz")
  
  extract_storage(
    archive = wacz,
    exdir = tmp
  )
  
  # Read the page list and WARC index.
  pages <- read_pages_jsonl(tmp)
  cdx <- read_cdx(tmp)
  
  
  # Match each pages.jsonl entry to its exact CDX capture.
  page_captures <- pages |>
    dplyr::mutate(
      cdx_timestamp = gsub(
        "[-:TZ.]",
        "",
        timestamp
      )
    ) |>
    dplyr::left_join(
      cdx,
      by = c(
        "resource_locator",
        "cdx_timestamp"
      )
    )
  
  
  # Find the records that actually contain HTML payloads.
  #
  # This also allows WARC revisit records to be resolved through
  # their shared payload digest.
  html_payloads <- cdx |>
    dplyr::filter(
      mime == "text/html"
    ) |>
    dplyr::select(
      payload_digest = digest,
      payload_warc_filename = warc_filename,
      payload_offset = offset,
      payload_length = length
    )
  
  
  review_pages <- page_captures |>
    dplyr::left_join(
      html_payloads,
      by = c(
        "digest" = "payload_digest"
      )
    ) |>
    dplyr::mutate(
      source = dplyr::case_when(
        grepl(
          "finna.fi",
          resource_locator
        ) ~ "finna",
        
        grepl(
          "muis.ee",
          resource_locator
        ) ~ "muis",
        
        grepl(
          "garamantas.lv",
          resource_locator
        ) ~ "garamantas",
        
        TRUE ~ "other"
      )
    )
  
  
  # Read the captured HTML for every page.
  html <- purrr::pmap_chr(
    list(
      review_pages$payload_warc_filename,
      review_pages$payload_offset,
      review_pages$payload_length
    ),
    function(
    warc_filename,
    offset,
    length
    ) {
      
      record <- read_wacz_record(
        root = tmp,
        warc_filename = warc_filename,
        offset = offset,
        length = length
      )
      
      extract_html_document(record)
    }
  )
  
  
  # Apply the source-specific representative image rule.
  thumbnail_url <- purrr::map2_chr(
    html,
    review_pages$source,
    extract_thumbnail_url
  )
  
  
  # Return the minimal input required by the curatorial review.
  #
  # Pages without a representative image are excluded. This removes
  # overview/search/navigation pages such as the Finna search result
  # encountered in the test collection.
  review_pages |>
    dplyr::transmute(
      page_id = page_id,
      title = title,
      page_url = resource_locator,
      thumbnail_url = thumbnail_url
    ) |>
    dplyr::filter(
      !is.na(thumbnail_url),
      thumbnail_url != ""
    )
}


# -------------------------------------------------------------------------
# Test
# -------------------------------------------------------------------------

wacz <- "D:/_assets/wacz/muis-udmurt-photographs.wacz"

curatorial_pages_udmurts <- prepare_wacz_review_pages(wacz)

curatorial_pages_udmurts

saveRDS(curatorial_pages_udmurts, 
        "D:/_assets/wacz/muis-udmurt-photographs.rds")



wacz <- "D:/_assets/wacz/women-in-latgale-1950s.wacz"

curatorial_pages <- prepare_wacz_review_pages(wacz)

curatorial_pages$thumbnail_url[1]


curatorial_pages
saveRDS(curatorial_pages, 
        "D:/_assets/wacz/women-in-latgale-1950s_curatorial_pages.rds")
