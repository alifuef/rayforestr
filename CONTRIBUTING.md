# Contributing to rayforestr

Thank you for contributing to `rayforestr`.

## Scope

`rayforestr` is an R package for stage-based forestry workflows built around
RayCloudTools and TreeTools through Docker. Contributions should aim to improve:

- reproducible pipeline execution
- stage-level and wrapper-level workflows
- import robustness
- documentation and examples
- test coverage
- summary and reader utilities

## Before you start

Please try to keep changes focused and small. A good contribution usually does
one of the following:

- fixes one bug
- improves one workflow
- adds tests for one behavior
- improves one documentation area

## Recommended development steps

1. Create a branch for your change.
2. Make the change in the smallest practical number of files.
3. Run documentation, tests, and checks locally.
4. Open a pull request with a clear summary.

## Local checks

Before submitting changes, run:

```r
devtools::document()
devtools::test()
devtools::check()
```

If your change affects Docker-based execution, also test at least one practical
workflow manually.

## Package conventions

### Pipeline design
- Prefer stage-local workflows over copy-heavy chaining.
- Use hard-link-first staging with copy fallback where appropriate.
- Keep stage folders clean and predictable.

### Public API
- Keep high-level workflows centered on:
  - `ray_pipeline_single_tree()`
  - `ray_pipeline_forest()`
- Add or export helper functions only when they are useful for users, not only
  for internal implementation.

### Documentation
- User-facing functions should have roxygen documentation.
- Workflow explanations should go into vignettes, not only function docs.
- Update `README.md` and `NEWS.md` when behavior changes substantially.

### Tests
- Add or update `testthat` tests for behavior changes.
- Prefer small, focused tests.
- When full Docker integration is not practical in automated tests, add logic
  tests and document any manual validation steps.

## Reporting bugs

When reporting a bug, please include:

- the function or wrapper used
- the input mode
- the operating system
- the Docker image used
- the exact command or R call
- the error message
- whether the issue occurs in a fresh run directory

## Pull request suggestions

A clear pull request should include:

- what changed
- why it changed
- how it was tested
- any user-facing consequences

## Notes

Large point-cloud data should generally stay outside the package repository.
Use lightweight examples and keep the package source focused on code,
documentation, tests, and vignettes.
