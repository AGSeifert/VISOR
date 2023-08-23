test_that("simplerspec ➡️ hyperSpec: opus", {
  skip_if_not_installed("simplerspec")
  skip_if_not_installed("tibble")
  skip_if_not_installed("hyperSpec")

  data(simplerspec_opus)

  skip_if_not(exists("simplerspec_opus"), "Example data set required for test")

  hy <- simplerspec_to_hyperSpec(simplerspec_opus)

  expect_s4_class(hy, "hyperSpec")
})
