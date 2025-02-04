species_list <- c(
  "Merluccius merluccius",
  "Gadus chalcogrammus",
  "Anoplopoma fimbria"
)
traits_example <- purrr::map_df(species_list, get_FishLife_traits)

usethis::use_data(traits_example, overwrite = TRUE)
