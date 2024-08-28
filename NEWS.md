# VISOR (development version)

* Add target formats: `to_ChemoSpec()`, `to_hyperSpec()`
  * `hyperSpec_to_ChemoSpec()`
  * `ChemoSpec_to_hyperSpec()`
* Add matrix source for ChemoSpec: `matrix_to_ChemoSpec()`
* Add simplerspec source for ChemoSpec: `simplerspec_to_ChemoSpec()`
* Add simplerspec source for hyperSpec: `simplerspec_to_hyperSpec()`
* Add opusreader2 source for hyperSpec: `opusreader2_to_hyperSpec()`
  * Add opusreader2 source for ChemoSpec via hyperSpec: `opusreader2_to_ChemoSpec()`
  * Add support functions `.getDataBlockNames()` and `.getParamBlockNames()`.

## Changes

* Fix call to `hyperSpec::new_hyperSpec` (was renamed to `hyperSpec::hyperSpec` in https://github.com/r-hyperspec/hyperSpec/commit/65c2c2d562769cf998d1ead950538089d3bf52d5).
* `to_ChemoSpec`-family of functions:
  * `groups` is no longer a required parameter.
  * `groups` now also accepts a single string or integer value, 
    which is then treated as the group for every sample.
  * `groups` now defaults to `"{missing group}`.
