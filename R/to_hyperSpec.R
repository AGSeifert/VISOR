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

#' {opusreader2} ⚪ ➡️ 🔵 {hyperSpec}
#'
#' Spectra are automatically grouped by wavenumbers in `data_block`, and a list
#' is returned. The spectra must then be resampled/aligned to match a common set
#' of wavenumbers before they can be combined.
#'
#' Some data will not be converted:
#' - Spectra columns except the `spc_column`
#' - Wavenumber columns except the `wavelength_column`
#'
#' @param or2 An `opusreader2_list`.
#' @inheritParams to_hyperSpec
#' @inheritDotParams hyperSpec::new_hyperSpec gc
#'
#' @returns A list of `hyperSpec::hyperSpec-class` objects.
#'
#' @examples
#' data("opusreader2_list")
#' spectra <- opusreader2_to_hyperSpec(opusreader2_list[[1]]) |> str()
#'
#' @export
#' @keywords from_opusreader2 to_hyperSpec
#' @seealso `to_hyperSpec()`
opusreader2_to_hyperSpec <- function(
  opusreader2_list,
  data_block = "ab",
  ...
) {
  rlang::check_installed("rlist")
  checkmate::assert_class(opusreader2_list, c("list_opusreader2", "list"))

  data_block <- data_block |> match.arg({
    # Finds blocks which are in all files
    blocks_tbl = opusreader2_list |> lapply(function(x) {
      is_block = x |> lapply(function(block) {
        !is.null(block$data)
      })
      is_block[which(as.logical(is_block))] |> names()
    }) |> unlist() |> table()

    blocks_tbl[which(blocks_tbl == length(opusreader2_list))] |> names()
  })

  param_blocks <- {
    # Finds blocks which are in all files
    params_tbl = opusreader2_list |> lapply(function(x) {
      is_block = x |> lapply(function(block) {
        !is.null(block$parameters)
      })
      is_block[which(as.logical(is_block))] |> names()
    }) |> unlist() |> table()

    params_tbl[which(params_tbl == length(opusreader2_list))] |> names()
  }

  opusreader2_list |>
    rlist::list.group(.[[data_block]]$wavenumbers) |>
    lapply(function(opusreader2_list) {
      wavelength <- opusreader2_list[[1]][[data_block]]$wavenumbers

      data <- {
        data_pre = opusreader2_list |> lapply(function(x) {
          x[param_blocks] |> lapply(function(block) {
            block$parameters |> lapply(function(param) {
              param$parameter_value
            })
          }) |> as.data.frame()
        })

        # This will drop meta data columns not included in EVERY spectra being loaded
        data_cols_to_keep = {
          data_cols_tbl = data_pre |> lapply(colnames) |> unlist() |> table()

          cols_to_drop = data_cols_tbl[which(data_cols_tbl != length(opusreader2_list))] |> names()

          if (cols_to_drop |> length() > 0) {
            warning(paste(
              "Dropping the following columns due to not being present in every file being loaded:\n·",
              data_cols_tbl[which(data_cols_tbl != length(opusreader2_list))] |> names() |> paste(collapse = "\n· ")
            ))
          }

          data_cols_tbl[which(data_cols_tbl == length(opusreader2_list))] |> names()
        }

        data_pre = data_pre |> lapply(function(df) {
          df[, data_cols_to_keep]
        })

        do.call("rbind", data_pre)
      }

      labels <- opusreader2_list[[1]][param_blocks] |> lapply(function(block) {
        block$parameters |> lapply(function(param) {
          param$parameter_name_long
        })
      }) |> unlist() |> as.list()
      labels$.wavelength <- "TODO"
      labels$spc <- "TODO"

      spc <- do.call(
        "rbind",
        opusreader2_list |> lapply(function(x) x[[data_block]]$data |> as.double())
      )

      checkmate::assert_matrix(
        spc, "numeric",
        nrows = opusreader2_list |> length(), ncols = wavelength |> length()
      )

      data$spc <- spc

      to_hyperSpec(
        data = data,
        wavelength = wavelength,
        labels = labels,
        ...
      )
    })
}

#' {simplerspec} ⚪ ➡️ 🔵 {hyperSpec}
#'
#' Some data will not be converted:
#' - Spectra columns except the `spc_column`
#' - Wavenumber columns except the `wavelength_column`
#'
#' @param spc_tbl [`simplerspec::gather_spc`],
#'        [`simplerspec::resample_spc`], [`simplerspec::average_spc`],
#'        or [`simplerspec::preprocess_spc`] spectra tibble.
#' @param spc_column Column name of the data to use.
#'        Must be one of `"spc_pre", "spc_mean", "spc_rs", "spc"`, or
#'        any column name if `strict_column_check` is set to FALSE.
#' @param wavelength_column Column name of the data to use.
#'        Must be one of `"xvalues_pre", "wavenumbers_rs", "wavenumbers"`, or
#'        any column name if `strict_column_check` is set to FALSE.
#' @param unit_wavelength,unit_spc **Optional** labels for the x- and y-axis.
#' @param .strict_column_check (**Default**: TRUE) Set this to FALSE to allow
#'        using any valid column name on `spc_tbl` in `spc_column` and
#'        `wavelength_column`.
#' @inheritParams to_hyperSpec
#' @inheritDotParams hyperSpec::new_hyperSpec gc
#'
#' @returns A `ChemoSpec::Spectra()` object.
#'
#' @examples
#' data("simplerspec_opus")
#' spectra <- simplerspec_to_hyperSpec(simplerspec_opus) |> str()
#'
#' @export
#' @keywords from_simplerspec to_hyperSpec
#' @seealso `to_hyperSpec()`, `simplerspec_to_ChemoSpec()`
simplerspec_to_hyperSpec <- function(
    spc_tbl,
    spc_column = base::c("spc_pre", "spc_mean", "spc_rs", "spc"),
    wavelength_column = base::c("xvalues_pre", "wavenumbers_rs", "wavenumbers"),
    unit_spc = "{missing spectra unit}",
    unit_wavelength = "{missing wavelength unit}",
    .strict_column_check = TRUE,
    ...) {
  rlang::check_installed("tibble")

  spc_tbl <- tibble::as_tibble(spc_tbl)

  checkmate::assert_class(spc_tbl, base::c("tbl_df", "tbl", "data.frame"))

  if (.strict_column_check) {
    spc_column <- base::match.arg(spc_column, c("spc_pre", "spc_mean", "spc_rs", "spc"))
    wavelength_column <- base::match.arg(wavelength_column, c("xvalues_pre", "wavenumbers_rs", "wavenumbers"))
  } else {
    spc_column <- base::match.arg(spc_column, colnames(spc_tbl))
    wavelength_column <- base::match.arg(wavelength_column, colnames(spc_tbl))
  }

  spc <- base::do.call(rbind, spc_tbl[, spc_column][[1]]) |> as.matrix()
  data <- base::do.call(rbind, spc_tbl$metadata) |> base::as.data.frame()
  wavelength <- spc_tbl[, wavelength_column][[1]][[1]]

  labels <- base::list(
    .wavelength = unit_wavelength,
    spc = unit_spc
  )

  for (data_name in colnames(data)) {
    labels[[data_name]] <- data_name
  }

  # Because simplerspec contains a metadata field beginning with "spc"
  # (spc_in_file), the check in hyperSpec for is.null(data$spc) will
  # return that metadata column instead of NULL.
  # To avoid causing an unnecessary warning message for package users,
  # the spectra data is instead passed as part of data.
  data$spc <- spc

  to_hyperSpec(
    data = data,
    wavelength = wavelength,
    labels = labels,
    ...
  )
}
