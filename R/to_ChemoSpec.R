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
