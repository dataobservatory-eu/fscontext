# fscontext 0.2.2

- Added a vignette demonstrating the transition from filesystem observation to human review with Betwixt.
- Added examples for reviewing candidate identity, source, and semantic relationships derived from `fscontext` observations.
- Added a workflow for preserving mappings between remote resource URIs and locally observed digital resources.
- Documented the use of intermediate relational projections for reproducible human revision of PROV- and RiC-O-oriented heuristics.

# fscontext 0.2.1

- Bug fix: when repo_root is empty, empty scans are returned.
- New vignette on file organisation.
- Released on CRAN

DOI v0.2.1: [10.5281/zenodo.21756596](https://doi.org/10.5281/zenodo.21756596)
DOI of the project [10.32614/CRAN.package.fscontext](https://doi.org/10.32614/CRAN.package.fscontext)

# 0.2.0

- Initial CRAN submission.
- Added support for observing ZIP archives using `scan_storage()`. 
- Added structural aggregation profiles (`folder-depth-1` to `folder-depth-4`
   and `wacz`) in `derive_structural_groups()`. 
- Added a vignette illustrating structural aggregation metadata, candidate 
  Record Sets, and RiC-inspired contextual reconstruction using directories,
  ZIP archives, and WACZ packages.
