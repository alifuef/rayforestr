test_that("path resolver creates standard stage layout", {
  td <- tempfile("rayforestr-test-")
  dir.create(td, recursive = TRUE)
  input <- file.path(td, "tree.ply")
  writeLines("ply", input)

  paths <- ray_resolve_paths(input_file = input, project_root = td, run_name = "demo")

  expect_true(dir.exists(paths$dirs$import))
  expect_true(dir.exists(paths$dirs$terrain))
  expect_true(dir.exists(paths$dirs$logs))
  expect_match(paths$out_root, "runs/demo$")
})

test_that("result status set includes wrapper statuses", {
  expect_true(all(c("success", "dry_run", "existing_input", "external_input", "not_requested") %in% ray_result_statuses()))
})
