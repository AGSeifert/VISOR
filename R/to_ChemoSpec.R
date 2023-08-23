#' *️⃣ {ChemoSpec}
#'
#' @param data Numeric matrix with samples in rows and frequencies in cols.
#' @param groups Factor with sample class assignments.
#'        Length must match `nrow()` of `data`.
#' @param freq Numeric vector with unique frequencies.
#'        Length must match `ncol()` of `data`.
#'        **Defaults to** `colnames()` of `data` cast `as.numeric()`.
#' @param names Character vector with unique sample names.
#'        Length must match `nrow()` of `data`.
#'        **Defaults to** `rownames()` of `data`.
#' @param unit_frequency,unit_intensity **Optional** labels for the x- and y-axis.
#' @param desc **Optional** description for plots generated with {ChemoSpec}.
#' @param colors_set,sym_set,alt.sym_set Character vector with group colors,
#'        Numeric vector with group symbol numbers, Character vector with
#'        alternative group symbols.
#'        Their length must match at least the number of levels in `groups`.
#'        Groups are assigned colors based on their numeric factor level.
#'        **Defaults to** `ChemoSpecUtils::Col8`, `ChemoSpecUtils::Sym8` and
#'        `base::letters`.
#'        A color-blind friendly palette is available in `ChemoSpecUtils::Col7`.
#'        Extended palettes are available at `ChemoSpecUtils::Col12`,
#'        `ChemoSpecUtils::Sym12`, and using other packages, like {RColorBrewer}.
#' @param extra_data **Optional** data frame with samples in rows and additional
#'        per-sample data in its columns. `colnames()` of `extra_data` is used to
#'        obtain names which will be used as `names()` on the `Spectra` object.
#' @param .strict_extra_data_names (**Default**: TRUE) Set this to FALSE to allow
#'        columns from `extra_data` to overwrite values on the `Spectra` object.
#'
#' @returns A `ChemoSpec::Spectra()` object.
#'
#' @export
#' @keywords to_ChemoSpec
to_ChemoSpec <- function(
    data,
    groups,
    freq = base::colnames(data) |> base::as.numeric(),
    names = base::rownames(data),
    desc = "{missing description}",
    unit_frequency = "{missing frequency unit}",
    unit_intensity = "{missing intensity unit}",
    colors_set = ChemoSpecUtils::Col8,
    sym_set = ChemoSpecUtils::Sym8,
    alt.sym_set = base::letters,
    extra_data = NULL,
    .strict_extra_data_names = TRUE) {
  rlang::check_installed("ChemoSpecUtils")
  checkmate::assert_matrix(
    data,
    mode = "numeric", nrows = base::length(names), ncols = base::length(freq)
  )
  checkmate::assert_numeric(
    freq,
    any.missing = FALSE, unique = TRUE, len = base::ncol(data)
  )
  checkmate::assert_character(
    names,
    any.missing = FALSE, unique = TRUE, len = base::nrow(data)
  )
  checkmate::assert_factor(
    groups,
    len = base::nrow(data), empty.levels.ok = FALSE, any.missing = FALSE, min.levels = 1
  )
  checkmate::assert_string(unit_frequency, min.chars = 1)
  checkmate::assert_string(unit_intensity, min.chars = 1)
  checkmate::assert_string(desc, min.chars = 1)
  checkmate::assert_character(
    colors_set,
    min.len = base::length(levels(groups)), unique = TRUE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    sym_set,
    min.len = base::length(levels(groups)), unique = TRUE, any.missing = FALSE
  )
  checkmate::assert_character(
    alt.sym_set,
    min.len = base::length(levels(groups)), unique = TRUE, any.missing = FALSE
  )
  checkmate::assert_data_frame(
    extra_data,
    null.ok = TRUE,
    nrows = base::length(names), row.names = "unique"
  )

  Spectra <- base::list()
  base::class(Spectra) <- "Spectra"

  Spectra$data <- data
  Spectra$freq <- freq
  Spectra$unit <- base::c(unit_frequency, unit_intensity)
  Spectra$desc <- desc

  Spectra$groups <- groups
  Spectra$names <- names

  Spectra$colors <- colors_set[groups]
  Spectra$sym <- sym_set[groups]
  Spectra$alt.sym <- alt.sym_set[groups]


  if (!base::is.null(extra_data)) {
    for (colname in base::colnames(extra_data)) {
      if (colname %in% base::c("freq", "names", "data", "groups", "colors", "sym", "alt.sym", "unit", "desc")) {
        msg <- paste0(
          "extra_data contains column with reserved name \"", colname, "\".",
        )
        ifelse(.strict_extra_data_names,
          stop(msg),
          warning(paste(
            msg,
            "This will overwrite the original information in the `Spectra` object.",
            "Proceed with caution, as this may cause unexpected behavior.",
            sep = "\n"
          ))
        )
      }
      Spectra[[colname]] <- extra_data[, colname]
    }
  }

  ChemoSpecUtils::chkSpectra(Spectra)

  return(Spectra)
}

#' Matrix ⚪ ➡️ 🟠 {ChemoSpec}
#'
#' @param data Numeric matrix with sample rows and frequency cols.
#'        `rownames()` must be unique and contain sample names.
#'        `colnames()` must be unique and contain frequency values, which can
#'        be cast to numeric using `as.numeric()`.
#' @inheritParams to_ChemoSpec
#' @inheritDotParams to_ChemoSpec desc unit_frequency unit_intensity colors_set sym_set alt.sym_set extra_data .strict_extra_data_names
#'
#' @returns A `ChemoSpec::Spectra()` object.
#'
#' @examples
#' x <- matrix(runif(260), nrow = 26)
#' rownames(x) <- letters[1:26]
#' colnames(x) <- 1:10
#' matrix_to_ChemoSpec(
#'   x,
#'   groups = rep(LETTERS[1:2], 13) |> as.factor()
#' ) |> str()
#'
#' @export
#' @keywords from_matrix to_ChemoSpec
#' @seealso `to_ChemoSpec()`
matrix_to_ChemoSpec <- function(
    data,
    groups,
    ...) {
  checkmate::assert_matrix(
    data,
    mode = "numeric", row.names = "unique", col.names = "unique"
  )

  to_ChemoSpec(
    data = data,
    freq = base::colnames(data) |> base::as.numeric(),
    names = base::rownames(data),
    groups = groups,
    ...
  )
}

#' {simplerspec} ⚪ ➡️ 🟠 {ChemoSpec}
#'
#' Some data will not be converted:
#' - Spectra columns except the `data_column`
#' - Wavenumber columns except the `freq_column`
#'
#' @param spc_tbl [`simplerspec::gather_spc`],
#'        [`simplerspec::resample_spc`], [`simplerspec::average_spc`],
#'        or [`simplerspec::preprocess_spc`] spectra tibble.
#' @param data_column Column name of the data to use.
#'        Must be one of `"spc_pre", "spc_mean", "spc_rs", "spc"`, or
#'        any column name if `strict_column_check` is set to FALSE.
#' @param freq_column Column name of the data to use.
#'        Must be one of `"xvalues_pre", "wavenumbers_rs", "wavenumbers"`, or
#'        any column name if `strict_column_check` is set to FALSE.
#' @param name_column Column name of the data to use.
#'        Must be one of `"unique_id", "file_id", "sample_id", "sample_name"`, or
#'        any metadata column name if `strict_column_check` is set to FALSE.
#' @param .name_column_collapse (**Default**: NULL) Optional string, that
#'        if set allows multiple metadata column names to be set in `name_column`,
#'        causing them to be concatenated using `.name_column_collapse` as a
#'        separator.
#'        This can be used with `.strict_column_check = FALSE` to construct
#'        custom sample identifiers using other metadata such as `rep_no`.
#' @param .strict_column_check (**Default**: TRUE) Set this to FALSE to allow
#'        using any valid column name on `spc_tbl` in `data_column` and
#'        `freq_column`, and any metadata column names in `name_column`.
#' @inheritParams to_ChemoSpec
#' @inheritDotParams to_ChemoSpec desc unit_frequency unit_intensity colors_set sym_set alt.sym_set .strict_extra_data_names
#'
#' @returns A `ChemoSpec::Spectra()` object.
#'
#' @examples
#' data("simplerspec_opus")
#' simplerspec_to_ChemoSpec(
#'   simplerspec_opus,
#'   groups = factor(LETTERS[1:3])
#' ) |> str()
#'
#' @export
#' @keywords from_simplerspec to_ChemoSpec
#' @seealso `to_ChemoSpec()`
simplerspec_to_ChemoSpec <- function(
    spc_tbl,
    groups,
    data_column = base::c("spc_pre", "spc_mean", "spc_rs", "spc"),
    freq_column = base::c("xvalues_pre", "wavenumbers_rs", "wavenumbers"),
    name_column = base::c("unique_id", "file_id", "sample_id", "sample_name"),
    .name_column_collapse = NULL,
    .strict_column_check = TRUE,
    ...) {
  rlang::check_installed("tibble")

  spc_tbl <- tibble::as_tibble(spc_tbl)

  checkmate::assert_class(spc_tbl, base::c("tbl_df", "tbl", "data.frame"))
  checkmate::assert_string(.name_column_collapse, null.ok = TRUE)

  if (.strict_column_check) {
    data_column <- base::match.arg(data_column, c("spc_pre", "spc_mean", "spc_rs", "spc"))
    freq_column <- base::match.arg(freq_column, c("xvalues_pre", "wavenumbers_rs", "wavenumbers"))
    name_column <- base::match.arg(name_column, c("unique_id", "file_id", "sample_id", "sample_name"), several.ok = !is.null(.name_column_collapse))
  } else {
    data_column <- base::match.arg(data_column, colnames(spc_tbl))
    freq_column <- base::match.arg(freq_column, colnames(spc_tbl))
    name_column <- base::match.arg(name_column, colnames(spc_tbl$metadata[[1]]), several.ok = !is.null(.name_column_collapse))
  }

  .duplicate_rows <- spc_tbl[, freq_column][[1]] |>
    base::duplicated()
  .sample_count <- spc_tbl[, freq_column][[1]] |>
    base::as.data.frame() |>
    base::nrow()
  .all_freq_identical <- base::sum(.duplicate_rows) == (.sample_count - 1)

  if (.all_freq_identical) {
    stop(base::paste0(
      "ChemoSpec requires that all spectra in a `Spectra` object have the same frequencies (x-values).\n",
      "Check that $", freq_column, " on your `simplerspec` contains only identical lists!\n",
      "Hint: The following list elements are not duplicates of the first one:\n",
      "[", which(!.duplicate_rows)[-1] |> paste(collapse = ", "), "]."
    ))
  }

  names <- spc_tbl$metadata |>
    base::lapply(function(x) base::paste(x[name_column], collapse = .name_column_collapse)) |>
    base::unlist()

  data <- base::do.call(rbind, spc_tbl[, data_column][[1]]) |> as.matrix()
  base::rownames(data) <- names

  freq <- spc_tbl[, freq_column][[1]][[1]]
  checkmate::assert_numeric(
    freq,
    any.missing = FALSE,
    len = base::ncol(data)
  )

  extra_data <- base::do.call(rbind, spc_tbl$metadata) |> base::as.data.frame()
  base::rownames(extra_data) <- names

  to_ChemoSpec(
    data = data,
    freq = freq,
    names = names,
    groups = groups,
    extra_data = extra_data,
    ...
  )
}

#' {hyperSpec} 🔵  ➡️ 🟠 {ChemoSpec}
#'
#' {ChemoSpec} requires that the {hyperSpec} object
#' meets the following conditions:
#' - Wavelengths must be unique.
#' - Spectra must not contain `NA`s.
#'
#' @param hySpc [`hyperSpec::hyperSpec-class`] object.
#' @inheritParams to_ChemoSpec
#' @inheritDotParams to_ChemoSpec groups desc colors_set sym_set alt.sym_set .strict_extra_data_names
#'
#' @examples
#' suppressPackageStartupMessages(library(hyperSpec))
#' data(flu)
#' hyperSpec_to_ChemoSpec(
#'   flu,
#'   names = 1:6 |> as.character(),
#'   groups = LETTERS[1:6] |> as.factor()
#' ) |> str()
#'
#' @export
#' @keywords from_hyperSpec to_ChemoSpec
#' @seealso `to_ChemoSpec()`, `ChemoSpec_to_hyperSpec()`
hyperSpec_to_ChemoSpec <- function(hySpc, names, groups, ...) {
  to_ChemoSpec(
    data = hySpc[[]],
    freq = hySpc |> hyperSpec::wl(),
    names = names,
    # ^ Unique filenames are not enforced by hyperSpec
    groups = groups,
    # ^ No class factor is required for hyperSpec
    unit_frequency = hySpc@label$.wavelength |> base::as.character(),
    unit_intensity = hySpc@label$spc |> base::as.character(),
    extra_data = hySpc$..,
    ...
  )
}
