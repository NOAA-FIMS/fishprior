#' Get traits for a species from the FishLife package
#'
#' Get life-history traits for a given species from the FishLife package. Look
#' at the example for how to get data on multiple species at once. This function
#' depends on the `FishBase_and_Morphometrics` data object from the FishLife
#' package.
#'
#' @param species A string specifying the scientific name for the species of
#'   interest. This function will only accept a single string. See the examples
#'   for how to get data on multiple species at once.
#' @param keep_regexp A string specifying a regular expression to search for.
#'   The default searches for multiple words using `"|"` as a separator. This
#'   default will return nine traits related to life history.
#'
#' @return
#' A data frame with four columns is returned. The first column, `name`, stores
#' the scientific/latin name for the species. The same value that was used in
#' the `species` input argument. The second column, `trait`, stores the name of
#' the life-history trait. Most of these are start with log( to specify that the
#' values are in natural log space. The third and fourth columns, `mean` and
#' `se`, store the mean and the standard error of the mean for the given trait.
#' @export
#' @examples
#' species_list <- c(
#'   "Merluccius merluccius",
#'   "Gadus chalcogrammus",
#'   "Anoplopoma fimbria"
#' )
#' get_FishLife_traits(species_list[1])
#' # Get just natural mortality information
#' get_FishLife_traits(species_list[1], keep_regexp = "mort")
#' # Expect error because the species PacFIN does not exist
#' \dontrun{
#' get_FishLife_traits("PacFIN")
#' }
#' # Expect error because the function is not vectorized
#' \dontrun{
#' get_FishLife_traits(species_list[1:2])
#' }
#' # Example of how to get vectorized output and save it
#' traits <- purrr::map_df(species_list, get_FishLife_traits)
#' \dontrun{
#' utils::write.csv(traits, file = "example_traits_FishLife.csv")
#' }
get_FishLife_traits <- function(
  species,
  keep_regexp = "age|fecundity|length_|mortality|weight|growth"
) {
  FishBase_and_Morphometrics <- FishLife::FishBase_and_Morphometrics
  data <- FishBase_and_Morphometrics
  if (length(species) > 1) {
    cli::cli_abort(
      message = "{.var species} can only have a length of 1 instead of
                {length(species)}."
    )
  }
  position <- match(species, data[["tree"]][["tip.label"]])
  if (length(position) == 0 | is.na(position)) {
    cli::cli_abort(
      message = "Your species {.var {species}} was not found in the available
                 list, perhaps you should double check the spelling. Use the
                 following code to view the species available:
                 {.code FishBase_and_Morphometrics$tree$tip.label}."
    )
  }
  trait <- NULL
  trait_mean <- data[["beta_gv"]][position, ]
  trait_se <- sqrt(diag(data[["Cov_gvv"]][position, , ]))
  trait_data <- tibble::tibble(
    name = species,
    trait = names(trait_mean),
    mean = trait_mean,
    se = trait_se
  )
  out_data_frame <- dplyr::filter(
    trait_data,
    grepl(keep_regexp, trait)
  )
  return(out_data_frame)
}
