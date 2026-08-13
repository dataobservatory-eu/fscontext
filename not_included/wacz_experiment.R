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


# -------------------------------------------------------------------------
# Explore the internal structure of a WACZ archive
# -------------------------------------------------------------------------

# PURPOSE
#
# This file is an exploratory investigation of WACZ as an observational
# source for fscontext. It is deliberately more verbose than production
# code: the objective is to understand which observations are present in
# a WACZ package, how they relate to one another, and which assumptions
# fscontext may safely make when ingesting WACZ archives.
#
#
# WHAT WE HAVE LEARNED
#
# 1. A WACZ is a ZIP-based package containing several distinct layers of
#    evidence. In the archives examined so far these include:
#
#      datapackage.json
#      datapackage-digest.json
#      pages/pages.jsonl
#      indexes/index.cdx
#      archive/data.warc.gz
#
#    These files should not be collapsed conceptually into a single
#    observation source. They describe different aspects of the capture.
#
#
# 2. datapackage.json describes the WACZ package and its constituent
#    resources. It provides package-level metadata such as the WACZ
#    version, creating software, timestamps, paths, sizes, and hashes.
#
#
# 3. pages/pages.jsonl is page-oriented.
#
#    It identifies resources that Webrecorder treats as pages and can
#    provide, among other things:
#
#      page identifier
#      resource locator
#      capture timestamp
#      page title
#      extracted text
#
#    It is therefore not simply another representation of the CDX index.
#
#
# 4. indexes/index.cdx is capture/resource-oriented.
#
#    It records individual captured resources and provides information
#    needed to locate their corresponding WARC records, including:
#
#      resource locator
#      capture timestamp
#      payload digest
#      MIME type
#      HTTP status
#      WARC filename
#      compressed byte offset
#      compressed byte length
#
#
# 5. A page and a CDX capture can be matched exactly using the resource
#    locator together with the capture timestamp.
#
#    For example:
#
#      pages.jsonl: 2026-06-24T17:53:27.878Z
#      index.cdx:   20260624175327878
#
#    These are different serialisations of the same capture time.
#
#
# 6. A resource locator is not a unique identifier for a WARC record.
#
#    The same resource may occur repeatedly in the CDX because it was
#    encountered during multiple captures. URL alone is therefore not a
#    sufficient key for identifying a particular capture.
#
#
# 7. Repeated CDX observations do not necessarily represent different
#    payload versions.
#
#    WACZ uses WARC revisit records when previously captured content is
#    encountered again. Revisit records can share a payload digest with
#    an earlier payload-bearing record.
#
#    Consequently:
#
#      repeated URL != necessarily changed content
#
#    and:
#
#      repeated capture != necessarily repeated stored payload
#
#
# 8. The payload digest provides an important relationship between
#    captures.
#
#    In the archives examined so far, a revisit capture can be associated
#    with the earlier payload-bearing record through the shared digest.
#    This allows the captured page observation to be distinguished from
#    the physical storage of its payload.
#
#
# 9. CDX offset and length refer to compressed byte ranges inside the
#    WARC gzip file.
#
#    The experiment below demonstrates that seeking directly to a CDX
#    offset and reading the specified number of bytes can yield an
#    independently decompressible WARC record.
#
#    This means that fscontext does not need to decompress or parse the
#    complete WARC merely to retrieve one indexed capture.
#
#
# 10. A WARC response record contains several observable layers:
#
#       WARC metadata
#       HTTP response metadata
#       captured payload
#
#     These should remain distinguishable in the observational model.
#
#
# 11. The curatorial ingestion experiments with Finna, MUIS, and
#     Garamantas show that representative images can be discovered from
#     the captured HTML, but the rule is source-specific:
#
#       Finna:
#         <meta property="og:image" ...>
#
#       MUIS:
#         <img name="thumbnail" ...>
#
#       Garamantas:
#         <img class="image-zoom" ...>
#
#     The generic WACZ layer therefore gives us the captured evidence,
#     while interpretation of a page's "representative image" belongs to
#     a source-specific extraction layer.
#
#
# 12. This distinction is important for fscontext.
#
#     WACZ ingestion should first preserve observations supplied by the
#     package and its WARC records. Statements such as "this image is the
#     representative image of this cultural object" are interpretations
#     derived from page structure and should not silently become generic
#     WACZ observations.
#
#
# CURRENT PRACTICAL RESULT
#
# For the immediate curatorial experiments we have a lightweight
# ingestion workflow producing:
#
#      page_id
#      title
#      page_url
#      thumbnail_url
#
# for curatorial pages captured from:
#
#      finna.fi
#      muis.ee
#      garamantas.lv
#
# This intentionally pragmatic ingestion can be used to gather curator
# experience while the more general fscontext WACZ observational model
# is developed separately.


# -------------------------------------------------------------------------
# NEXT INVESTIGATIONS
# -------------------------------------------------------------------------

# A. WACZ package integrity
#
# Inspect datapackage-digest.json and establish precisely how package
# integrity is represented and how it relates to the hashes recorded in
# datapackage.json.
#
# TODO:
#   - parse datapackage-digest.json;
#   - verify the digest of datapackage.json;
#   - verify hashes of constituent WACZ resources;
#   - determine which verification results should become observations.


# B. WARC record structure
#
# Replace the current exploratory raw-byte/text inspection with explicit
# parsing of:
#
#   WARC headers
#   HTTP headers
#   payload
#
# Investigate which fields should be retained by fscontext without
# imposing archival semantics on them.


# C. Revisit records
#
# Formalise the relationship currently observed experimentally:
#
#   page capture
#       -> revisit WARC record
#       -> shared payload digest
#       -> payload-bearing WARC record
#
# Test this against additional WACZ archives before treating the observed
# behaviour as an ingestion invariant.


# D. Capture identity
#
# Determine the appropriate observational key for a captured resource.
#
# Candidates include combinations of:
#
#   WARC record ID
#   WARC page ID
#   resource locator
#   capture timestamp
#   payload digest
#
# Do not assume that URL or digest alone identifies a capture.


# E. Time observations
#
# Inventory the different timestamps available across:
#
#   datapackage.json
#   pages.jsonl
#   index.cdx
#   WARC headers
#   HTTP headers
#
# Determine what event or observation each timestamp actually describes
# before mapping any of them to higher-level temporal concepts.


# F. Payload and record digests
#
# Investigate the distinction between:
#
#   payload digest
#   block/record digest
#   datapackage resource hash
#
# These identify integrity at different layers and should not be treated
# as interchangeable signatures.


# G. Multiple WARC files
#
# Current examples use archive/data.warc.gz.
#
# Test ingestion against a WACZ containing multiple WARC files and make
# sure all retrieval logic uses warc_filename from the CDX rather than
# assuming data.warc.gz.


# H. Non-HTML resources
#
# Investigate observational treatment of:
#
#   images
#   PDFs
#   audio/video
#   CSS/JavaScript
#   fonts
#   redirects
#   failed HTTP responses
#
# The current curatorial experiment is intentionally page-centred and
# should not determine the general fscontext WACZ model.


# I. Source-specific curatorial extraction
#
# Keep Finna, MUIS, and Garamantas extraction rules separate from generic
# WACZ ingestion.
#
# TODO:
#   - test the three extractors on additional curator-created WACZ files;
#   - record extraction failures rather than silently dropping them;
#   - decide how source-specific handlers should eventually be registered;
#   - add further sources only when curator use cases require them.


# J. Curatorial review experiment
#
# Use the current minimal ingestion immediately with real reviewers.
#
# Gather evidence about whether reviewers actually need:
#
#   page title
#   source page URL
#   representative image
#
# and what additional context they request during review.
#
# Do not design the final review interface solely from assumptions made
# during WACZ engineering.


# K. General fscontext observational model
#
# Once the WACZ evidence layers are understood, determine how they fit
# the wider fscontext observational model.
#
# Preserve the separation between:
#
#   source evidence
#   observations
#   structural/contextual relationships
#   semantic interpretation
#   curatorial review
#
# Mapping to Records, Record Parts, Instantiations, RiC-O, PROV-O, or
# other semantic models should remain downstream of raw WACZ observation
# unless the WACZ itself explicitly supplies such semantics.