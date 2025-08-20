species_list <- c(
  "Merluccius merluccius",
  "Gadus chalcogrammus",
  "Anoplopoma fimbria"
)
traits_example <- get_fishbase_traits(spec_names = species_list) |>
  summarize_fishbase_traits()

usethis::use_data(traits_example, overwrite = TRUE)
