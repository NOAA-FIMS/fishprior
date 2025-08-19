species_list <- c(
  "Merluccius merluccius",
  "Gadus chalcogrammus",
  "Anoplopoma fimbria"
)
traits_example <- summarize_fishlife_traits(species_list)

usethis::use_data(traits_example, overwrite = TRUE)
