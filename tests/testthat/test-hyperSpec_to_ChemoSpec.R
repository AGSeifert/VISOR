test_that("hyperSpec ➡️ ChemoSpec: flu", {
  skip_if_not_installed("hyperSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(hyperSpec)
  })

  data(flu)

  suppressMessages({
    hyperSpec_to_ChemoSpec(
      flu,
      names = 1:6 |> as.character(),
      groups = LETTERS[1:6] |> as.factor()
    )
  })

  expect(TRUE, "Should stop if chkSpectra fails")
})

test_that("hyperSpec ➡️ ChemoSpec: laser", {
  skip_if_not_installed("hyperSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(hyperSpec)
  })

  data(laser)

  suppressMessages({
    hyperSpec_to_ChemoSpec(
      laser,
      names = 1:84 |> as.character(),
      groups = rep("A", 84) |> as.factor()
    )
  })

  expect(TRUE, "Should stop if chkSpectra fails")
})

test_that("hyperSpec ➡️ ChemoSpec: paracetamol", {
  skip("ChemoSpec does not support duplicate frequencies")
})

test_that("hyperSpec ➡️ ChemoSpec: barbiturates", {
  skip_if_not_installed("hyperSpec")
  skip_if_not_installed("ChemoSpecUtils")

  suppressPackageStartupMessages({
    library(hyperSpec)
  })

  data(barbiturates)

  suppressMessages({
    b <- barbiturates |> hyperSpec::collapse()
    b[[is.na(b)]] <- 0

    hyperSpec_to_ChemoSpec(
      b,
      names = 1:5 |> as.character(),
      groups = LETTERS[1:5] |> as.factor()
    )
  })

  expect(TRUE, "Should stop if chkSpectra fails")
})
