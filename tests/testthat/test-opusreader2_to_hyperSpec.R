test_that("opusreader2 ➡️ hyperSpec: opus", {
  skip_if_not_installed("opusreader2")
  skip_if_not_installed("hyperSpec")

  data(opusreader2_list)

  skip_if_not(exists("opusreader2_list"), "Example data set required for test")

  hy <- opusreader2_list |> opusreader2_to_hyperSpec()

  hy |> lapply(expect_s4_class, "hyperSpec")
})

test_that("opusreader2 ➡️ hyperSpec: opus-dbg", {
  skip_if_not_installed("opusreader2")
  skip_if_not_installed("hyperSpec")

  opusreader2_list <- suppressWarnings(opusreader2::read_opus(
    file.path(usethis::proj_path(), "data-raw", "opus-dbg")
  ))

  hy <- opusreader2_list |> opusreader2_to_hyperSpec()

  hy |> lapply(expect_s4_class, "hyperSpec")
})
