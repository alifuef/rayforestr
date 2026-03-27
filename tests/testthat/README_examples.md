# README workflow examples

## Start from a raw point cloud

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3.ply",
  project_root = "D:/rayproject",
  use_colour = FALSE
)
```

## Start from an existing raycloud

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/data/single_tree/beech_3_raycloud.ply",
  input_mode = "raycloud",
  project_root = "D:/rayproject",
  steps = c("terrain", "trees", "smooth", "info", "mesh")
)
```

## Start from an existing treefile

```r
res <- ray_pipeline_single_tree(
  input_file = "D:/rayproject/runs/demo/step5_trees/beech_3_raycloud_trees.txt",
  input_mode = "treefile",
  project_root = "D:/rayproject",
  steps = c("smooth", "info", "mesh")
)
```

## Resume later steps only

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

## Forest wrapper from an existing raycloud

```r
res <- ray_pipeline_forest(
  input_file = "D:/rayproject/data/plot/site01_raycloud.ply",
  input_mode = "raycloud",
  project_root = "D:/rayproject",
  steps = c("terrain", "trees", "split", "smooth", "info", "mesh")
)
```
