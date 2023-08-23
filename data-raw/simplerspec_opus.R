## code to prepare `simplerspec_opus` dataset goes here

simplerspec_opus <- file.path(usethis::proj_path(), "data-raw", "opus") |>
  dir(full.names = TRUE) |>
  simplerspec::read_opus_univ() |>
  simplerspec::gather_spc(spc_types = "spc") |>
  simplerspec::resample_spc(wn_lower = 4000, wn_upper = 11500, wn_interval = 4) |>
  simplerspec::average_spc() |>
  simplerspec::preprocess_spc(select = "custom", custom_function = "spc_raw")

usethis::use_data(simplerspec_opus, overwrite = TRUE)
