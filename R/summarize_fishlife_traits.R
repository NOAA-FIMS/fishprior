#' Summarize FishLife trait data into log-transformed traits
#'
#' FishLife already summarizes these traits for us so this function is just
#' returning a table for a given species from the FishLife package. This
#' function depends on the `FishBase_and_Morphometrics` data object from the
#' FishLife package.
#'
#' @param species A vector of strings specifying the scientific names for the
#'   species of interest.
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
#' @seealso
#' * [summarize_fishbase_traits()]
#'
#' @examples
#' species_list <- c(
#'   "Merluccius merluccius",
#'   "Gadus chalcogrammus",
#'   "Anoplopoma fimbria"
#' )
#' summarize_fishlife_traits(species_list)
#' # Get just natural mortality information
#' summarize_fishlife_traits(species_list, keep_regexp = "mort")
#' # Expect error because the species PacFIN does not exist
#' \dontrun{
#' summarize_fishlife_traits("PacFIN")
#' }
summarize_fishlife_traits <- function(
    species,
    keep_regexp = "age|fecundity|length_|mortality|weight|growth") {
  FishBase_and_Morphometrics <- FishLife::FishBase_and_Morphometrics
  data <- FishBase_and_Morphometrics

  purrr::map_df(
    species_list,
    summarize_fishlife_trait,
    data = data,
    keep_regexp = keep_regexp
  )
}

#' Get the FishLife traits for a single species
#'
#' @inheritParams summarize_fishlife_traits
#' @param data A data frame returned by [FishLife::FishBase_and_Morphometrics].
#' @noRd
summarize_fishlife_trait <- function(species, keep_regexp, data) {
  # TODO: Move this check to the top of summarize_fishlife_traits so that the
  #       function will fail fast and informative for all species that are
  #       missing rather than over and over again if a vector is provided.
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
    Species = species,
    trait = names(trait_mean),
    mean = trait_mean,
    se = trait_se
  ) |>
    dplyr::filter(
      grepl(keep_regexp, trait)
    ) |>
    transform_data_frame()
  # TODO: get mean_normal and sd_normal
}
