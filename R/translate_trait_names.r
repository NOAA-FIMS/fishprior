#' Change FishBase trait name to snake-case descriptions
#'
#' The resultant trait names are the snake-case names that are used in
#' FishLife and are preceded by `log(` to indicate that the traits are in
#' log space.
#'
#' @param trait A character vector of trait names from FishBase.
#' @examples
#' translate_trait_names("Loo")
#' translate_trait_names("K")
#' translate_trait_names(c("Loo", "K", "M"))
#' @export
#' @return
#' A character vector of trait names in snake_case format rather than
#' the format found in FishBase, which is often shorted versions of the trait
#' names.
translate_trait_names <- function(trait) {
  dplyr::case_when(
    trait == "tmax" ~ "log(age_max)",
    trait == "FecundityMean" ~ "log(fecundity)",
    trait == "K" ~ "log(growth_coefficient)",
    trait == "Lmax" ~ "log(length_max)",
    trait == "Loo" ~ "log(length_infinity)",
    trait == "Lm" ~ "log(length_maturity)",
    trait == "tm" ~ "log(age_maturity)",
    trait == "M" ~ "log(natural_mortality)",
    TRUE ~ trait  # Return the original trait name if no match found
  )
}
