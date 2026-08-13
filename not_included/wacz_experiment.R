# -------------------------------------------------------------------------
# Explore the internal structure of a WACZ archive
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# 1. Locate and extract the example WACZ
# -------------------------------------------------------------------------

# Locate the example WACZ distributed with fscontext.
wacz <- system.file(
  "testdata", "fscontext_020.wacz",
  package = "fscontext"
)

# Create a temporary directory in which to inspect the archive contents.
tmp <- tempfile("wacz")

# A WACZ is a ZIP-based package. Extract it so that its constituent
# files can be examined independently.
extract_storage(
  wacz,
  exdir = tmp
)

# List the complete package structure.
#
# In this example the WACZ contains:
#   archive/data.warc.gz       captured WARC records
#   datapackage-digest.json    digest information for the package
#   datapackage.json           WACZ package metadata
#   indexes/index.cdx          index of captured resources
#   pages/pages.jsonl          page-level metadata
list.files(
  tmp,
  recursive = TRUE
)


# -------------------------------------------------------------------------
# 2. Inspect the WACZ data package
# -------------------------------------------------------------------------

# Read datapackage.json.
#
# This describes the WACZ package itself, including its WACZ version,
# creating software, creation/modification timestamps, and constituent
# resources.
datapackage <- read_datapackage(tmp)

str(datapackage)


# -------------------------------------------------------------------------
# 3. Inspect the CDX resource index
# -------------------------------------------------------------------------

# Read indexes/index.cdx.
#
# The CDX index describes captured resources in the WARC. Its entries
# contain resource locators, capture timestamps, digests, MIME types,
# HTTP status codes, and the location of the corresponding WARC records.
cdx <- read_cdx(tmp)

# Examine the structure and dimensions of the resulting observational table.
names(cdx)
nrow(cdx)
dplyr::glimpse(cdx)


# Examine the types of resources represented in the archive.
#
# Note that "warc/revisit" entries represent subsequent encounters with
# resources whose payloads need not be stored again in full.
cdx |>
  dplyr::count(mime, sort = TRUE)


# Determine which WARC files contain the indexed records.
#
# This example WACZ contains a single WARC file.
cdx |>
  dplyr::count(warc_filename, sort = TRUE)


# Count how many CDX entries occur for each resource locator.
#
# A resource may occur repeatedly because it was encountered during
# several page captures. Repeated CDX entries therefore should not
# automatically be interpreted as different "versions" of a resource.
cdx |>
  dplyr::count(resource_locator, sort = TRUE)


# Count occurrences of each payload digest.
#
# This helps distinguish repeated encounters with the same content from
# observations involving different payload content.
cdx |>
  dplyr::count(digest, sort = TRUE)


# Inspect the complete capture histories of resource locators that occur
# more than once.
#
# Sorting by URL and capture timestamp makes original payload records and
# subsequent revisit records easier to compare.
cdx |>
  dplyr::filter(
    duplicated(resource_locator) |
      duplicated(resource_locator, fromLast = TRUE)
  ) |>
  dplyr::arrange(
    resource_locator,
    cdx_timestamp
  )


# -------------------------------------------------------------------------
# 4. Inspect page-level metadata
# -------------------------------------------------------------------------

# Read pages/pages.jsonl.
#
# Unlike the CDX index, which describes captured resources, pages.jsonl
# identifies resources that Webrecorder treats as archived pages and
# supplies page-oriented information such as identifiers, titles,
# timestamps, and extracted text.
pages <- read_pages_jsonl(tmp)

# Display the principal identifying fields for the archived pages.
pages |>
  dplyr::select(
    page_id,
    resource_locator,
    timestamp,
    title
  )


# -------------------------------------------------------------------------
# 5. Identify full HTML captures in the CDX
# -------------------------------------------------------------------------

# Select CDX entries containing full HTML responses.
#
# This deliberately excludes WARC revisit records. For this example
# archive there are six full HTML captures.
html <- cdx |>
  dplyr::filter(mime == "text/html") |>
  dplyr::select(
    resource_locator,
    cdx_timestamp,
    digest,
    offset,
    length,
    status
  ) |>
  dplyr::arrange(cdx_timestamp)

print(
  html,
  n = Inf,
  width = Inf
)


# -------------------------------------------------------------------------
# 6. Test the relationship between pages.jsonl and HTML captures
# -------------------------------------------------------------------------

# Check whether pages.jsonl contains the same resource locator more
# than once.
pages |>
  dplyr::count(resource_locator) |>
  dplyr::filter(n > 1)


# Perform the same test for the full HTML CDX records.
html |>
  dplyr::count(resource_locator) |>
  dplyr::filter(n > 1)


# Find page entries for which no corresponding full HTML CDX record
# exists.
#
# An empty result means that every page in pages.jsonl has a matching
# HTML capture by resource locator.
pages |>
  dplyr::anti_join(
    html,
    by = "resource_locator"
  )


# Test the relationship in the opposite direction.
#
# An empty result means that every full HTML CDX record also corresponds
# to a page represented in pages.jsonl.
html |>
  dplyr::anti_join(
    pages,
    by = "resource_locator"
  )


# -------------------------------------------------------------------------
# 7. Compare page metadata with CDX capture metadata
# -------------------------------------------------------------------------

# Join the page-level and capture-level observations.
#
# The two timestamp fields are retained separately so that their
# relationship can be inspected rather than assumed.
page_html <- pages |>
  dplyr::select(
    page_id,
    resource_locator,
    page_timestamp = timestamp,
    title
  ) |>
  dplyr::left_join(
    html |>
      dplyr::rename(
        capture_timestamp = cdx_timestamp
      ),
    by = "resource_locator"
  )

page_html |>
  dplyr::select(
    page_id,
    resource_locator,
    page_timestamp,
    capture_timestamp,
    title
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# In this example the two timestamp representations correspond exactly:
#
# pages.jsonl:
#   2026-06-24T17:53:27.878Z
#
# index.cdx:
#   20260624175327878
#
# Thus pages.jsonl and index.cdx provide complementary descriptions of
# the same six page captures.


# -------------------------------------------------------------------------
# 8. Locate the compressed WARC
# -------------------------------------------------------------------------

# Construct the path to the WARC file identified by the CDX entries.
warc_gz <- file.path(
  tmp,
  "archive",
  "data.warc.gz"
)

# Confirm that the WARC exists and inspect its compressed size.
file.exists(warc_gz)
file.info(warc_gz)$size


# -------------------------------------------------------------------------
# 9. Use a CDX offset and length to retrieve one WARC record
# -------------------------------------------------------------------------

# The CDX entry for the fscontext home page reports:
#
#   resource_locator = https://fscontext.dataobservatory.eu/
#   offset           = 290
#   length           = 6445
#
# Test whether these values identify an independently decompressible
# record inside data.warc.gz.

# Open the compressed WARC as an ordinary binary file.
#
# It is important to use file(), rather than gzfile(), here because the
# CDX offset refers to a position in the compressed WARC file.
con <- file(
  warc_gz,
  "rb"
)

# Move to the compressed byte offset recorded in index.cdx.
seek(
  con,
  where = 290,
  origin = "start"
)

# Read exactly the number of compressed bytes specified by the CDX
# record.
compressed_record <- readBin(
  con,
  what = "raw",
  n = 6445
)

close(con)

# Confirm that the requested number of bytes was retrieved.
length(compressed_record)


# -------------------------------------------------------------------------
# 10. Decompress the selected WARC record independently
# -------------------------------------------------------------------------

# Save the selected compressed byte range as a temporary gzip file.
record_gz <- tempfile(
  fileext = ".warc.gz"
)

con <- file(
  record_gz,
  "wb"
)

writeBin(
  compressed_record,
  con
)

close(con)

# Its size should correspond to the CDX length.
file.info(record_gz)$size


# Attempt to decompress this byte range independently.
#
# If successful, this demonstrates that the CDX offset and length can
# provide random access to the corresponding gzip-compressed WARC record.
con <- gzfile(
  record_gz,
  "rb"
)

record <- readBin(
  con,
  what = "raw",
  n = 1000000
)

close(con)

# Inspect the size of the decompressed record.
length(record)

# Display the WARC record, including its WARC headers, HTTP response
# headers, and captured payload.
cat(
  rawToChar(record)
)


# -------------------------------------------------------------------------
# 11. Decompress the complete WARC for exploratory inspection
# -------------------------------------------------------------------------

# The indexed random-access experiment above retrieves one record.
# For comparison, decompress the complete WARC into a temporary file.
warc <- tempfile(
  fileext = ".warc"
)

in_con <- gzfile(
  warc_gz,
  "rb"
)

out_con <- file(
  warc,
  "wb"
)

# Copy the decompressed stream in 1 MiB chunks rather than loading the
# entire WARC into memory at once.
repeat {
  chunk <- readBin(
    in_con,
    "raw",
    n = 1024 * 1024
  )
  
  if (length(chunk) == 0) {
    break
  }
  
  writeBin(
    chunk,
    out_con
  )
}

close(in_con)
close(out_con)

# Inspect the size of the complete decompressed WARC.
file.info(warc)$size


# -------------------------------------------------------------------------
# 12. Inspect the beginning of the decompressed WARC
# -------------------------------------------------------------------------

# Read only the first 10,000 bytes initially.
con <- file(
  warc,
  "rb"
)

warc_start <- readBin(
  con,
  "raw",
  n = 10000
)

close(con)

# Display the beginning of the WARC as text.
#
# This exposes the WARC headers and, for response records, the embedded
# HTTP headers and payload.
cat(
  rawToChar(warc_start)
)


# -------------------------------------------------------------------------
# 13. Locate the fscontext home page within the decompressed WARC
# -------------------------------------------------------------------------

# Read the decompressed WARC as lines for exploratory searching.
#
# This is convenient for a small test archive, although a production
# parser should not assume that arbitrary WARC payloads are UTF-8 text.
warc_lines <- readLines(
  warc,
  warn = FALSE,
  encoding = "UTF-8"
)

# Find lines containing the target URL.
#
# Use the literal URL here; markdown link syntax is not part of the
# stored WARC content.
grep(
  "https://fscontext.dataobservatory.eu/",
  warc_lines,
  value = TRUE,
  fixed = TRUE
)


# Obtain the positions of those matches.
hits <- grep(
  "https://fscontext.dataobservatory.eu/",
  warc_lines,
  fixed = TRUE
)

hits


# -------------------------------------------------------------------------
# 14. Inspect the WARC context surrounding the first URL match
# -------------------------------------------------------------------------

# Select the first occurrence of the URL.
i <- hits[1]

# Display ten lines before and thirty lines after the match.
#
# This allows the WARC headers, HTTP headers, and nearby payload content
# to be inspected in context.
warc_lines[
  max(1, i - 10):
    min(length(warc_lines), i + 30)
]