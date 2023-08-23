# https://r-hyperspec.github.io/hyperSpec/reference/initialize.html

#' *️⃣ {hyperSpec}
#'
#' @inheritDotParams hyperSpec::new_hyperSpec spc data wavelength labels gc
#'
#' @returns A [`hyperSpec::hyperSpec-class`] object.
#'
#' @export
#' @keywords to_hyperSpec
to_hyperSpec <- function(...) {
  rlang::check_installed("hyperSpec", version = "0.200.0.9000")
  hyperSpec::new_hyperSpec(...)
}
