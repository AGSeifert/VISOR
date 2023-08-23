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

#' {ChemoSpec} 🟠 ➡️ 🔵 {hyperSpec}
#'
#' @param Spectra `ChemoSpec::Spectra()` object.
#' @inheritDotParams hyperSpec::new_hyperSpec gc
#'
#' @returns A [`hyperSpec::hyperSpec-class`] object.
#'
#' @examples
#' suppressPackageStartupMessages(library(ChemoSpec))
#' data("alignMUD")
#' ChemoSpec_to_hyperSpec(alignMUD) |> str()
#'
#' @export
#' @keywords from_ChemoSpec to_hyperSpec
#' @seealso `to_hyperSpec()`, `hyperSpec_to_ChemoSpec()`
ChemoSpec_to_hyperSpec <- function(Spectra, ...) {
  rlang::check_installed("ChemoSpecUtils")
  ChemoSpecUtils::chkSpectra(Spectra)

  spc <- Spectra$data

  extra_names <- base::names(Spectra) |>
    base::setdiff(base::c(
      "freq", "names", "data", "groups", "colors", "sym", "alt.sym", "unit", "desc"
    ))

  data <- data.frame(
    groups = Spectra$groups,
    colors = Spectra$colors,
    sym = Spectra$sym,
    alt.sym = Spectra$alt.sym
  ) |> cbind(
    Spectra[extra_names] |>
      base::as.data.frame(row.names = Spectra$names)
  )

  wavelength <- Spectra$freq
  labels <- base::list(
    .wavelength = Spectra$unit[1],
    spc = Spectra$unit[2]
  )

  for (extra_name in extra_names) {
    labels[[extra_name]] <- extra_name
  }

  to_hyperSpec(
    spc = spc,
    data = data,
    wavelength = wavelength,
    labels = labels,
    ...
  )
}
