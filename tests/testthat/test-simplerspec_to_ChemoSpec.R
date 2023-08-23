test_that("simplerspec ➡️ ChemoSpec: opus", {
  skip_if_not_installed("simplerspec")
  skip_if_not_installed("tibble")
  skip_if_not_installed("ChemoSpecUtils")

  data(simplerspec_opus)

  skip_if_not(exists("simplerspec_opus"), "Example data set required for test")

  suppressMessages({
    simplerspec_to_ChemoSpec(
      simplerspec_opus,
      groups = factor(LETTERS[1:3])
    )
  })

  expect(TRUE, "Should stop if chkSpectra fails")
})
