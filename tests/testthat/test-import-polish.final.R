test_that("ray_import can prefer an existing raycloud in dry run", {
  td <- tempfile("rayforestr-import-")
  dir.create(td, recursive = TRUE)
  raw_input <- file.path(td, "tree.ply")
  existing_rc <- file.path(td, "tree_raycloud.ply")
  writeLines("ply", raw_input)

  res <- ray_import(
    input_file = raw_input,
    project_root = td,
    out_dir = file.path(td, "runs", "import"),
    existing_raycloud = existing_rc,
    reuse_existing = "prefer",
    dry_run = TRUE,
    echo = FALSE
  )

  expect_s3_class(res, "ray_result")
  expect_identical(res$status, "external_input")
  expect_identical(res$outputs$raycloud, normalizePath(existing_rc, winslash = "/", mustWork = FALSE))
})

test_that("ray_import failure diagnostics mention existing raycloud candidates", {
  msg <- ray_import_failure_message(
    input_file = "x/tree.ply",
    expected = "x/runs/tree_raycloud.ply",
    issue = "zero_intensity_nonreturns",
    existing_candidates = "x/tree_raycloud.ply",
    output = c("Error: ray cloud has no identified points;", "all rays are zero-intensity non-returns")
  )

  expect_true(grepl("Existing raycloud candidate", msg))
  expect_true(grepl("zero-intensity", msg))
  expect_true(grepl("input_mode = 'raycloud'", msg, fixed = TRUE))
})


test_that("ray_import fallback reports external input semantics in dry run-prefer path metadata", {
  res <- ray_external_input_result(
    step = "ray_import",
    input = "x/tree.ply",
    outputs = list(raycloud = "x/tree_raycloud.ply"),
    details = list(import_attempted = TRUE, import_failed = TRUE, fallback_used = TRUE, fallback_source = "sibling_raycloud")
  )

  expect_identical(res$status, "external_input")
  expect_true(isTRUE(res$details$fallback_used))
  expect_identical(res$details$fallback_source, "sibling_raycloud")
})
