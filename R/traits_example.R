#' Example trait information for three species
#'
#' An example of trait information obtained from FishBase for three
#' species, European Hake, Alaska pollock, and sablefish.
#'
#' @format ## `who`
#' A data frame with 19 rows and 6 columns:
#' \describe{
#'   \item{Species}{The latin/scientific name of the species}
#'   \item{trait}{A string in an R-code style indicating which trait the values
#'                are for}
#'   \item{mean_normal, sd_normal}{The mean and standard deviation of the given
#'                                 trait on the normal scale}
#'   \item{mean, se}{The mean and standard error of the given trait}
#' }
#' @source <https://docs.ropensci.org/rfishbase/>
"traits_example"
