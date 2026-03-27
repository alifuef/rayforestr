test_that("single-tree dry run supports point cloud entry", {
  td <- tempfile("rayforestr-dry-")
  dir.create(td, recursive = TRUE)
  input <- file.path(td, "tree.ply")
  writeLines("ply", input)

  res <- ray_pipeline_single_tree(
    input_file = input,
    project_root = td,
    run_name = "dryrun_single",
    dry_run = TRUE,
    echo = FALSE
  )

  expect_s3_class(res, "ray_result")
  expect_identical(res$status, "dry_run")
  expect_true(grepl("_raycloud\\.ply$", res$outputs$raycloud_file))
})

test_that("forest dry run supports raycloud entry mode", {
  td <- tempfile("rayforestr-dry-")
  dir.create(td, recursive = TRUE)
  input <- file.path(td, "tree_raycloud.ply")
  writeLines("ply", input)

  res <- ray_pipeline_forest(
    input_file = input,
    input_mode = "raycloud",
    project_root = td,
    run_name = "dryrun_forest",
    dry_run = TRUE,
    echo = FALSE
  )

  expect_s3_class(res, "ray_result")
  expect_identical(res$status, "dry_run")
  expect_true(grepl("_trees\\.txt$", res$outputs$treefile))
})
