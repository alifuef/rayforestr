test_that("image helpers fall back correctly", {
  old_rc <- getOption("rayforestr.raycloud_image")
  old_tt <- getOption("rayforestr.treetools_image")
  on.exit({
    options(rayforestr.raycloud_image = old_rc)
    options(rayforestr.treetools_image = old_tt)
  }, add = TRUE)

  options(rayforestr.raycloud_image = "example/ray:latest")
  options(rayforestr.treetools_image = NULL)

  expect_identical(ray_default_raycloud_image(), "example/ray:latest")
  expect_identical(ray_default_treetools_image(), "example/ray:latest")

  imgs <- ray_get_images()
  expect_identical(imgs$raycloud_image, "example/ray:latest")
  expect_identical(imgs$treetools_image, "example/ray:latest")
})
