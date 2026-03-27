# Recommended GitHub and repository structure for rayforestr

Use this as a practical layout for the package repository.

## Recommended top-level structure

```text
rayforestr/
  .github/
    workflows/
  R/
  man/
  tests/
    testthat/
  vignettes/
  inst/
    examples/
  data/
  DESCRIPTION
  NAMESPACE
  NEWS.md
  README.md
  LICENSE
  LICENSE.md
  _pkgdown.yml
```

## What each folder is for

### `.github/workflows/`
Use this for GitHub Actions such as:
- R-CMD-check
- pkgdown site build
- test runs

### `R/`
All package source files.

Suggested organization inside `R/`:
- `pipeline_*.R` for wrappers
- `steps_*.R` for stage functions
- `read_*.R` for readers
- `metrics.R`, `volume.R`, `forest_summary.R` for summaries
- `docker_utils_complete_fixed.R` and `setup.R` for infrastructure

### `man/`
Generated documentation files. Do not edit manually if you use roxygen.

### `tests/testthat/`
Unit tests and behavior tests.

### `vignettes/`
Long-form workflow documentation.

Current recommended vignettes:
- `getting-started-single-tree.Rmd`
- `forest-plot-workflow.Rmd`
- `import-strategies-troubleshooting.Rmd`
- `advanced-programmatic-use.Rmd`

### `inst/examples/`
Optional small example scripts or templates for users.

### `data/`
Only include small example data if needed. Avoid large point clouds here.

## Suggested GitHub workflow files

A good minimal set is:

```text
.github/workflows/
  R-CMD-check.yaml
  pkgdown.yaml
```

## Suggested release workflow

For each new release:

1. Update `DESCRIPTION` version
2. Update `NEWS.md`
3. Run:
   - `devtools::document()`
   - `devtools::test()`
   - `devtools::check()`
4. Commit changes
5. Tag the release in Git
6. Optionally build/update pkgdown site

## Suggested branch strategy

Simple and practical:
- `main` for stable code
- short-lived feature branches for changes
- tag stable releases such as `v0.0.8`

## Suggested future additions

Optional files you may add later:
- `CONTRIBUTING.md`
- `CODE_OF_CONDUCT.md`
- `.gitignore`
- `cran-comments.md` if you ever prepare for CRAN
- `pkgdown/` output folder if you publish a site

## Notes for rayforestr specifically

- Keep large forestry point clouds outside the package source tree.
- Use project roots such as `D:/packageR` or another workspace for real runs.
- The package source directory should contain code, docs, tests, and vignettes, not heavy processing outputs.
