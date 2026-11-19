
## 0.2.3

This is a minor maintenance release with no new functionality or changes to the public API.

The release addresses edge-case bugs identified through testing and experience with very large filesystem deployments on Windows.

The fixes improve:

- Git repository attribution when directory names share common prefixes.
- Isolation of temporary extraction directories when scanning ZIP archives.
- Protection against accidental overwriting of existing filesystem snapshots.
- Reliability of snapshot persistence when writes are interrupted.
- Validation of snapshot schema versions when reading persisted observations.

Existing snapshot schemas and deterministic observation identifiers remain unchanged to preserve backward compatibility.

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
