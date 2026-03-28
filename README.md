<img src="man/figures/logo.png" align="right" height="400" />

# rayforestr

`rayforestr` is an R package for stage-based forestry workflows built around
RayCloudTools and TreeTools through Docker. It provides:

- high-level wrappers for single-tree and forest/plot workflows
- step-level functions for import, terrain, trees, split, smooth, info, and mesh
- resumable workflows with selective stages
- multiple entry modes (`point_cloud`, `raycloud`, `treefile`, `smoothed_treefile`)
- R-native readers and analysis helpers for RayExtract outputs

## Installation

You can install `rayforestr` directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("alifuef/rayforestr")
```

To install a specific tagged release:

```r
install.packages("remotes")
remotes::install_github("alifuef/rayforestr@v0.0.8")
```

## Prerequisites

Pipeline execution requires Docker.

Before running workflows, make sure Docker is installed and available from R:

```r
library(rayforestr)
ray_check_docker()
```

## Image setup

When the RayCloudTools image already contains `treesmooth`, `treemesh`, and
`treeinfo`, you can use a single image for both RayCloudTools and TreeTools:

```r
options(
  rayforestr.raycloud_image = "ghcr.io/csiro-robotics/raycloudtools:latest"
)
```

`rayforestr` will let TreeTools fall back to the RayCloudTools image when no
separate TreeTools image is configured.

## Main entry modes

The main pipeline wrappers support these `input_mode` values:

- `"point_cloud"` for a raw `.ply`
- `"raycloud"` for an existing `*_raycloud.ply`
- `"treefile"` for an existing `*_trees.txt`
- `"smoothed_treefile"` for an existing `*_smoothed.txt`

## Workflow examples

### Start from a raw point cloud

```r
res <- ray_pipeline_single_tree(
  input_file   = "D:/rayproject/data/single_tree/beech_3.ply",
  input_mode   = "point_cloud",
  project_root = "D:/rayproject",
  use_colour   = FALSE
)
```

### Start from an existing raycloud

```r
res <- ray_pipeline_single_tree(
  input_file   = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  input_mode   = "raycloud",
  project_root = "D:/rayproject",
  steps        = c("terrain", "trees", "smooth", "info", "mesh")
)
```

### Start from an existing treefile

```r
res <- ray_pipeline_single_tree(
  input_file   = "D:/rayproject/runs/demo/step5_trees/beech_3_raycloud_trees.txt",
  input_mode   = "treefile",
  project_root = "D:/rayproject",
  steps        = c("smooth", "info", "mesh")
)
```

### Start from an existing smoothed treefile

```r
res <- ray_pipeline_single_tree(
  input_file   = "D:/rayproject/runs/demo/step7_smooth/beech_3_raycloud_trees_smoothed.txt",
  input_mode   = "smoothed_treefile",
  project_root = "D:/rayproject",
  steps        = c("info", "mesh")
)
```

### Resume later steps only

```r
res <- ray_pipeline_single_tree(
  input_file   = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  input_mode   = "raycloud",
  project_root = "D:/rayproject",
  run_name     = "beech_3_resume",
  steps        = c("smooth", "info", "mesh"),
  resume       = TRUE
)
```

### Forest wrapper from an existing raycloud

```r
res <- ray_pipeline_forest(
  input_file   = "D:/rayproject/data/plot/site01_raycloud.ply",
  input_mode   = "raycloud",
  project_root = "D:/rayproject",
  steps        = c("terrain", "trees", "split", "smooth", "info", "mesh")
)
```

## Import strategies

`ray_import()` and the wrappers support three practical import reuse modes:

- `"never"`: always attempt a fresh import
- `"prefer"`: use an existing raycloud immediately
- `"fallback"`: try fresh import first, then reuse an existing raycloud if needed

Example:

```r
res <- ray_pipeline_single_tree(
  input_file              = "D:/rayproject/data/single_tree/beech_3.ply",
  input_mode              = "point_cloud",
  project_root            = "D:/rayproject",
  reuse_existing_import   = "fallback",
  search_sibling_raycloud = TRUE,
  use_colour              = FALSE,
  steps                   = c("import", "terrain", "trees")
)
```

In practice, RayCloudTools may sometimes print import warnings while still
creating a usable `*_raycloud.ply`. `rayforestr` also supports practical
recovery paths through existing-raycloud reuse and fallback workflows.

## Pipeline rules

- The forest/plot pipeline uses `raycolour` when requested.
- Single-tree and corrected-tree pipelines do not use `raycolour` by default.
- `treeinfo` runs after `treesmooth`.

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

## Development notes

For stage execution, the package uses a link-first strategy:

- create a stage-local hard link when possible
- fall back to file copy if linking fails
- run the tool inside the stage folder
- keep only the true outputs in the stage directory

Useful helper functions during development and debugging include:

- `ray_default_raycloud_image()`
- `ray_default_treetools_image()`
- `ray_get_images()`
- `ray_set_images()`
- `ray_resolve_paths()`
- `ray_resolve_input_mode()`
- `ray_result_statuses()`

## Current scope

The package currently includes:

- Docker/path helpers
- step wrappers for `rayimport`, `raycolour`, `rayextract terrain`,
  `rayextract trees`, `raysplit`, `treesmooth`, `treeinfo`, and `treemesh`
- forest, single-tree, and corrected-tree pipelines
- batch helpers
- treefile and treeinfo parsers
- QSM-like conversion and volume helpers
