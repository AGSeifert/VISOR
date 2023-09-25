test_that("ChemoSpec ➡️ hyperSpec: SrE.IR", {
  suppressWarnings(skip_if_not_installed("hyperSpec"))
  skip_if_not_installed("ChemoSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(ChemoSpec)
  })

  data("SrE.IR")

  hy <- ChemoSpec_to_hyperSpec(SrE.IR)

  expect_s4_class(hy, "hyperSpec")
})

test_that("ChemoSpec ➡️ hyperSpec: SrE.NMR", {
  suppressWarnings(skip_if_not_installed("hyperSpec"))
  skip_if_not_installed("ChemoSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(ChemoSpec)
  })

  data("SrE.NMR")


  hy <- ChemoSpec_to_hyperSpec(SrE.NMR)

  expect_s4_class(hy, "hyperSpec")
})

test_that("ChemoSpec ➡️ hyperSpec: metMUD1", {
  suppressWarnings(skip_if_not_installed("hyperSpec"))
  skip_if_not_installed("ChemoSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(ChemoSpec)
  })

  data("metMUD1")

  hy <- ChemoSpec_to_hyperSpec(metMUD1)

  expect_s4_class(hy, "hyperSpec")
})

test_that("ChemoSpec ➡️ hyperSpec: metMUD2", {
  suppressWarnings(skip_if_not_installed("hyperSpec"))
  skip_if_not_installed("ChemoSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(ChemoSpec)
  })

  data("metMUD2")

  hy <- ChemoSpec_to_hyperSpec(metMUD2)

  expect_s4_class(hy, "hyperSpec")
})

test_that("ChemoSpec ➡️ hyperSpec: alignMUD", {
  suppressWarnings(skip_if_not_installed("hyperSpec"))
  skip_if_not_installed("ChemoSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(ChemoSpec)
  })

  data("alignMUD")

  hy <- ChemoSpec_to_hyperSpec(alignMUD)

  expect_s4_class(hy, "hyperSpec")
})
