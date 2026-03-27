test_that("input mode guessing works", {
  expect_identical(ray_resolve_input_mode(NULL, "x/spruce_4_raycloud.ply"), "raycloud")
  expect_identical(ray_resolve_input_mode(NULL, "x/tree_trees.txt"), "treefile")
  expect_identical(ray_resolve_input_mode(NULL, "x/tree_trees_smoothed.txt"), "smoothed_treefile")
  expect_identical(ray_resolve_input_mode(NULL, "x/raw_tree.ply"), "point_cloud")
})

test_that("default step sets follow input mode", {
  expect_true(all(c("terrain", "trees", "smooth", "info", "mesh") %in% ray_default_steps_single_tree("raycloud")))
  expect_identical(ray_default_steps_single_tree("treefile"), c("smooth", "info", "mesh"))
  expect_identical(ray_default_steps_single_tree("smoothed_treefile"), c("info", "mesh"))
  expect_true(all(c("split", "smooth", "info", "mesh") %in% ray_default_steps_forest("raycloud")))
})
