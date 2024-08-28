## code to prepare `opusreader2_list` dataset goes here

opusreader2_list <- opusreader2::read_opus(file.path(usethis::proj_path(), "data-raw", "opus"))

usethis::use_data(opusreader2_list, overwrite = TRUE)
