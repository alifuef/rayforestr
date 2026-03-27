# rayforestr

R package for orchestrating RayCloudTools and TreeTools from R through Docker,
plus R-native parsing and analysis helpers for RayExtract treefiles.

Current scope:

- Docker/path helpers
- step wrappers for `rayimport`, `raycolour`, `rayextract terrain`, `rayextract trees`, `raysplit`, `treesmooth`, `treeinfo`, and `treemesh`
- forest, single-tree, and corrected-tree pipelines
- batch helpers
- treefile and treeinfo parsers
- QSM-like conversion and volume helpers

## Pipeline rules

- Forest plot pipeline uses `raycolour`
- Single-tree and corrected-tree pipelines do not use `raycolour` by default
- `treeinfo` runs after `treesmooth`

## Install source package

On Windows, install from the source package or use `devtools::load_all()` / `devtools::install()` on the package folder during development.

## Image setup

When the RayCloudTools image already contains `treesmooth`, `treemesh`, and `treeinfo`, you can use a single image for both RayCloudTools and TreeTools:

```r
options(
  rayforestr.raycloud_image = "ghcr.io/csiro-robotics/raycloudtools:latest"
)
```

The package will let TreeTools fall back to the RayCloudTools image.

Useful image helpers during development:

```r
ray_default_raycloud_image()
ray_default_treetools_image()
ray_get_images()
```



## Vignettes

The package includes workflow-oriented vignettes for common use cases.

### Getting started and single-tree workflow
Covers:
- Docker setup
- image options
- recommended project structure
- single-tree processing from:
  - point clouds
  - existing rayclouds
  - existing treefiles
  - existing smoothed treefiles
- selective steps and resume

### Forest and plot workflow
Covers:
- forest/plot processing with `ray_pipeline_forest()`
- terrain, trees, split, smooth, info, and mesh stages
- selective steps and resume
- split outputs and stage directories

### Import strategies and troubleshooting
Covers:
- `reuse_existing = "never"`
- `reuse_existing = "prefer"`
- `reuse_existing = "fallback"`
- sibling raycloud fallback
- noisy import behavior
- practical import troubleshooting

### Advanced and programmatic use
Covers:
- calling step functions directly
- understanding `ray_result`
- path helpers and input-mode helpers
- selective steps, resume, and batch-style workflows

To build and view vignettes locally:

```r
devtools::build_vignettes("D:/packageR/rayforestr_0.0.8")
browseVignettes("rayforestr")
```

## Main entry modes

The pipeline wrappers support these `input_mode` values:

- `"point_cloud"` for a raw `.ply`
- `"raycloud"` for an existing `*_raycloud.ply`
- `"treefile"` for an existing `*_trees.txt`
- `"smoothed_treefile"` for an existing `*_smoothed.txt`

## Import behavior

`ray_import()` and the pipeline wrappers support three practical import strategies through
`reuse_existing` in `ray_import()` and `reuse_existing_import` in the wrappers.

### 1. Fresh import only

Always attempt a new raw import from the point cloud.

```r
res <- ray_import(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  project_root = "D:/rayproject",
  out_dir = "D:/rayproject/runs/import_demo/step2_rayimport",
  reuse_existing = "never"
)
```

Use this when you want a strict fresh run and do not want the package to reuse an existing raycloud.

### 2. Prefer an existing raycloud

Reuse an existing raycloud immediately and skip the raw import attempt.

```r
res <- ray_import(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  project_root = "D:/rayproject",
  out_dir = "D:/rayproject/runs/import_demo/step2_rayimport",
  existing_raycloud = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  reuse_existing = "prefer"
)
```

Use this when you already trust the existing `*_raycloud.ply` and want the fastest path forward.

### 3. Try import first, then fall back

Attempt a fresh raw import first, but if that does not produce a usable raycloud,
reuse an existing raycloud instead.

```r
res <- ray_import(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  project_root = "D:/rayproject",
  out_dir = "D:/rayproject/runs/import_demo/step2_rayimport",
  reuse_existing = "fallback",
  search_sibling_raycloud = TRUE
)
```

This is the most practical setting when some raw single-tree `.ply` files are known to be unstable with `rayimport`.

## Important note on `rayimport`

RayCloudTools may sometimes print warnings or error-like messages during import, including messages about zero-intensity non-returns, while still producing a usable `*_raycloud.ply`.

For that reason, `rayforestr` checks for the expected raycloud output file instead of relying only on console text. In practice:

- a raw import can print warnings and still be usable
- if raw import does not yield a usable raycloud and fallback is allowed, the wrappers can continue from an existing raycloud
- for difficult single-tree inputs, `reuse_existing_import = "fallback"` is often the safest wrapper setting

## Wrapper import options

The wrappers expose the same idea through:

- `input_mode`
- `reuse_existing_import`
- `search_sibling_raycloud`

Typical resilient single-tree example:

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  input_mode = "point_cloud",
  project_root = "D:/rayproject",
  reuse_existing_import = "fallback",
  search_sibling_raycloud = TRUE,
  use_colour = FALSE,
  steps = c("import", "terrain", "trees")
)
```

Forest wrapper example with the same import fallback logic:

```r
res <- ray_pipeline_forest(
  input_file = "D:/rayproject/data/plot/site01.ply",
  input_mode = "point_cloud",
  project_root = "D:/rayproject",
  reuse_existing_import = "fallback",
  search_sibling_raycloud = TRUE,
  use_colour = FALSE,
  steps = c("import", "terrain", "trees", "split")
)
```

## Workflow examples

### Start from a raw point cloud

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  project_root = "D:/rayproject",
  use_colour = FALSE
)
```

### Start from an existing raycloud

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  input_mode = "raycloud",
  project_root = "D:/rayproject",
  steps = c("terrain", "trees", "smooth", "info", "mesh")
)
```

### Start from an existing treefile

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/runs/demo/step5_trees/beech_3_raycloud_trees.txt",
  input_mode = "treefile",
  project_root = "D:/rayproject",
  steps = c("smooth", "info", "mesh")
)
```

### Start from an existing smoothed treefile

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/runs/demo/step7_smooth/beech_3_raycloud_trees_smoothed.txt",
  input_mode = "smoothed_treefile",
  project_root = "D:/rayproject",
  steps = c("info", "mesh")
)
```

### Resume later steps only

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  input_mode = "raycloud",
  project_root = "D:/rayproject",
  run_name = "beech_3_resume",
  steps = c("smooth", "info", "mesh"),
  resume = TRUE
)
```

### Forest wrapper from an existing raycloud

```r
res <- ray_pipeline_forest(
  input_file = "D:/rayproject/data/plot/site01_raycloud.ply",
  input_mode = "raycloud",
  project_root = "D:/rayproject",
  steps = c("terrain", "trees", "split", "smooth", "info", "mesh")
)
```

## Suggested helper functions to export

These are useful during development and debugging:

- `ray_default_raycloud_image()`
- `ray_default_treetools_image()`
- `ray_get_images()`
- `ray_set_images()`
- `ray_resolve_paths()`
- `ray_resolve_input_mode()`
- `ray_result_statuses()`

## Development note

For stage execution, the package now uses a link-first strategy:

- create a stage-local hard link when possible
- fall back to file copy if linking fails
- run the tool inside the stage folder
- keep only true outputs in the stage directory
