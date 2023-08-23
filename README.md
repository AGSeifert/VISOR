# **V**ery **I**nteroperable **S**pectra **O**bjects for **R**

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**VISOR** helps facilitate transformation of spectral data into various
open source data formats (like `ChemoSpec` and `hyperSpec`), both
from different file readers as well as between data formats.
This allows interoperability between all pre-processing, plotting, and 
data analysis workflows developed with different packages in mind.

## Supported data formats

The supported packages are split into two groups: Target and Source formats.

**Target** formats are the primary formats being converted to.
Every target format must have a function converting it to every other target format directly.

Current target formats are:

- `ChemoSpec`
- `hyperSpec`

**Source** formats are additional formats, such as from data-loading packages that open specific file formats (Opus, Excel, CSV).
Every source format must convert to at least one target format.
They may convert to multiple target formats through nested conversions, that is converting to a format by being converted to other compatible formats first.

Current source formats are:

- `simplerspec`
