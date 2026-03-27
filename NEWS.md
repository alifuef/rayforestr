# rayforestr 0.0.8

## Major changes

- Refactored the RayExtract execution model from copy-heavy stage chaining to a stage-local workflow.
- Added link-first staging of inputs with copy fallback when linking is not available.
- Improved cleanup so temporary localized inputs are removed after each step.
- Added selective step execution and resume support for pipeline wrappers.
- Added support for multiple pipeline entry modes:
  - `point_cloud`
  - `raycloud`
  - `treefile`
  - `smoothed_treefile`

## New pipeline behavior

- `ray_pipeline_single_tree()` now supports:
  - selective `steps`
  - `resume = TRUE/FALSE`
  - `stop_after`
  - starting from existing rayclouds or treefiles
- `ray_pipeline_forest()` now supports the same selective-step and resume workflow.
- TreeTools commands can now use the same Docker image as RayCloudTools when available in that image.

## Import improvements

- Polished `ray_import()` behavior for difficult point-cloud inputs.
- Added support for:
  - `reuse_existing = "never"`
  - `reuse_existing = "prefer"`
  - `reuse_existing = "fallback"`
- Added sibling-raycloud recovery for cases where raw import fails but an existing `*_raycloud.ply` is available.
- Improved import robustness in both single-tree and forest wrappers.

## Step-level improvements

- Refactored and validated:
  - `ray_terrain()`
  - `ray_trees()`
  - `ray_smooth()`
  - `ray_info()`
  - `ray_mesh()`
  - `ray_split_segmented()`
- Standardized stage outputs so each step folder keeps only the intended outputs.

## Documentation and package quality

- Updated README with new workflow examples and import-mode guidance.
- Added roxygen documentation for main wrappers and import behavior.
- Added exported helper documentation for advanced/programmatic use.
- Added testthat coverage for:
  - image resolution
  - input modes
  - path/result helpers
  - pipeline dry-run behavior

## Checks

- Package tests pass.
- `R CMD check` passes with 0 errors and 0 warnings.

## Notes

- Some raw single-tree point clouds may still trigger RayCloudTools import warnings or degenerate import behavior.
- The package now supports practical recovery paths for those cases through existing-raycloud reuse and fallback workflows.
