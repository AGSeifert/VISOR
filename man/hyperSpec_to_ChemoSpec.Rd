% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_ChemoSpec.R
\name{hyperSpec_to_ChemoSpec}
\alias{hyperSpec_to_ChemoSpec}
\title{{hyperSpec} 🔵  ➡️ 🟠 {ChemoSpec}}
\usage{
hyperSpec_to_ChemoSpec(hySpc, names, groups, ...)
}
\arguments{
\item{hySpc}{\code{\link[hyperSpec:hyperSpec-class]{hyperSpec::hyperSpec}} object.}

\item{names}{Character vector with unique sample names.
Length must match \code{nrow()} of \code{data}.
\strong{Defaults to} \code{rownames()} of \code{data}.}

\item{groups}{Factor with sample class assignments.
Length must match \code{nrow()} of \code{data}.}

\item{...}{
  Arguments passed on to \code{\link[=to_ChemoSpec]{to_ChemoSpec}}
  \describe{
    \item{\code{desc}}{\strong{Optional} description for plots generated with {ChemoSpec}.}
    \item{\code{colors_set,sym_set,alt.sym_set}}{Character vector with group colors,
Numeric vector with group symbol numbers, Character vector with
alternative group symbols.
Their length must match at least the number of levels in \code{groups}.
Groups are assigned colors based on their numeric factor level.
\strong{Defaults to} \code{ChemoSpecUtils::Col8}, \code{ChemoSpecUtils::Sym8} and
\code{base::letters}.
A color-blind friendly palette is available in \code{ChemoSpecUtils::Col7}.
Extended palettes are available at \code{ChemoSpecUtils::Col12},
\code{ChemoSpecUtils::Sym12}, and using other packages, like {RColorBrewer}.}
    \item{\code{.strict_extra_data_names}}{(\strong{Default}: TRUE) Set this to FALSE to allow
columns from \code{extra_data} to overwrite values on the \code{Spectra} object.}
  }}
}
\description{
{ChemoSpec} requires that the {hyperSpec} object
meets the following conditions:
\itemize{
\item Wavelengths must be unique.
\item Spectra must not contain \code{NA}s.
}
}
\examples{
suppressPackageStartupMessages(library(hyperSpec))
data(flu)
hyperSpec_to_ChemoSpec(
  flu,
  names = 1:6 |> as.character(),
  groups = LETTERS[1:6] |> as.factor()
) |> str()

}
\seealso{
\code{to_ChemoSpec()}, \code{ChemoSpec_to_hyperSpec()}
}
\keyword{from_hyperSpec}
\keyword{to_ChemoSpec}
