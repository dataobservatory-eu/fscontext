## Resubmission

This is the first CRAN submission of `fscontext`. From the pre-test the following was corrected:

- missing GPL-3 LICENSE

- Two misformatted URLs. URLs have been checked with `urlchecker::url_check()`.

- `.Rbuildignore` now ignores `cran-comments.md`

## Package purpose

`fscontext` provides a provenance-aware framework for contextual reconstruction from filesystem observations. It creates reproducible snapshots of file-level metadata, paths, repository context, and optional content signatures, and supports contextual grouping, structural abstraction, semantic stabilisation, duplicate detection, and reconstruction-oriented analysis.

The package is intended for analytical workflows involving research infrastructures, software repositories, digital collections, preservation environments, and other contexts where filesystem observations provide evidence about activities, resources, and their relationships.

The package is inspired by archival contextualisation approaches, particularly the Records in Contexts (RiC) family of conceptual models, but it is not an implementation of RiC-CM or RiC-O.

## Test environments

Local:

- Windows 11, R 4.5.x

Continuous integration:

- Ubuntu Linux (GitHub Actions)

- macOS (GitHub Actions)

- Windows (GitHub Actions)

Additional checks:

- R-hub checks on the standard CRAN target platforms.

## R CMD check results

There are no ERRORs, WARNINGs, or NOTEs.

## Notes

The package includes two small example filesystem snapshots (`fscontextdemo_snapshot_01` and `fscontextdemo_snapshot_02`) that are used in examples and vignettes. These are synthetic demonstration datasets created specifically for reproducible package documentation and testing.

The package does not access network resources during examples, tests, or vignette builds.
