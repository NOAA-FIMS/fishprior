#' Retrieve FishBase Life-History Trait Data for a Species
#'
#' Queries FishBase via the \pkg{rfishbase} package and returns raw data tables
#' containing life-history traits relevant for meta-analysis or prior construction.
#'
#' @param spec_names A character vector of species names (e.g., `"Gadus morhua"`).
#'
#' @return A named list with five tibbles:
#' \describe{
#'   \item{`popgrowth`}{Von Bertalanffy growth parameters, including \code{Loo}, \code{K}, and \code{M}.}
#'   \item{`popchar`}{Population characteristics such as \code{Lmax}, \code{tmax}, and \code{Wmax}.}
#'   \item{`poplw`}{Length-weight relationship parameters (\code{a}, \code{b}).}
#'   \item{`maturity`}{Maturity information including age and length at maturity.}
#'   \item{`fecundity`}{Fecundity data including \code{FecundityMean}, \code{FecundityMin}, and \code{FecundityMax}.}
#' }
#'
#' @importFrom rfishbase popgrowth popchar poplw maturity fecundity
#' @importFrom dplyr select
#' @export
get_fishbase_traits <- function(spec_names = NULL) {
  # Loo	Asymptotic length (L∞) in the von Bertalanffy Growth Function (typically in cm)
  # Number	Number of individuals used to estimate the growth parameters
  # SE_Loo	Standard error of Loo
  # SD_Loo	Standard deviation of Loo
  # K	Growth coefficient (K) from the von Bertalanffy model (in 1/year)
  # SE_K	Standard error of K
  # SD_K	Standard deviation of K
  # to	Theoretical age at zero length (t₀) from the von Bertalanffy model (in years)
  # SE_to	Standard error of to
  # SD_to	Standard deviation of to
  # Type	Type of length used in the growth model (e.g., "TL" = total length, "SL" = standard length, etc.). Important for consistency with poplw() or popchar()
  # M	Natural mortality rate (M) estimated from the growth model or accompanying source
  # SE_M	Standard error of M
  # SD_M	Standard deviation of M
  # YearStart	First year of data collection
  # YearEnd	Last year of data collection
  growth <- popgrowth(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, Sex, PopGrowthRef, DataSourceRef,Locality,
                  Loo, Number, SE_Loo, SD_Loo, K, SE_K, SD_K, to,
                  SE_to, SD_to, Type, M, SE_M, SD_M, YearStart, YearEnd)

  # Wmax	Maximum observed body weight in the population (usually in grams)
  # Lmax	Maximum observed length in the population (usually in cm)
  # tmax	Maximum observed age (in years)
  char <- popchar(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, Sex, SourceRef,
                  Wmax, Lmax, tmax, Locality)

  # lf <- poplf(species_list = spec_names) |>
  #   dplyr::select(Species, SpecCode, StockCode,
  #                 Locality, Sex)

  # LengthMax	Maximum length observed in the dataset used to fit the length-weight relationship (same units as Type, usually cm)
  # Type	Length type used (e.g., "TL" for Total Length, "FL" for Fork Length, "SL" for Standard Length). You’ll want to ensure consistency if pooling across studies
  # Number	Number of individuals used to fit the length-weight model
  # Sex	Sex category of the sample ("male", "female", or "mixed")
  # a	Intercept of the length-weight relationship: W = a * L^b
  # b	Slope (allometric coefficient) of the length-weight relationship. Often ~3 for isometric growth
  # SEa	Standard error of parameter a
  # SEb	Standard error of parameter b
  # SDa	Standard deviation of parameter a
  # SDb	Standard deviation of parameter b
  lw <- poplw(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode,
                  LengthMax, Type, Number, Sex,
                  a, b, SEa, SEb, SDa, SDb, Locality)

  # tm	Age at maturity (in years), interpreted as the age when 50% are mature
  # Number	Sample size used in the study providing tm and/or Lm
  # SE_tm	Standard error of age at maturity tm
  # SD_tm	Standard deviation of age at maturity tm
  # Lm	Length at maturity (typically in cm), often L50 (length at 50% maturity)
  # SD_Lm	Standard deviation of length at maturity Lm
  # SE_Lm	Standard error of length at maturity Lm
  mat <- maturity(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode, Sex,Locality,
                  AgeMatRef, tm, Number, SE_tm, SD_tm,
                  Lm, SD_Lm, SE_Lm
    )

  fecund <- fecundity(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode, Locality,
                  FecundityMin, FecundityMax, FecundityMean,
                  FecundityType, Number, a, b, SEa, SEb,
                  SDa, SDb)

  return(list(popgrowth = growth, popchar = char, poplw = lw, maturity = mat, fecundity = fecund))
}

#' Summarize FishBase Trait Data into Log-Transformed Traits
#'
#' Takes the output of \code{get_fishbase_traits()} and summarizes it into log-transformed life-history traits.
#' The returned tibble includes species, trait name, mean (on log-scale), and approximate SE.
#'
#' @param fb A list returned by \code{get_fishbase_traits()}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{`species`}{Species name.}
#'   \item{`trait`}{Name of the trait (e.g., \code{"log(length_infinity)"}).}
#'   \item{`mean_normal`}{Mean of the trait in normal space}
#'   \item{`sd_norma`}{Approximate standard deviation of the trait in normal space.}
#'   \item{`mean`}{Mean of the log-transformed trait}
#'   \item{`sd`}{Approximate standard deviation of the log-transformed trait.}
#' }
#'
#' @importFrom tibble tibble
#' @export
summarize_fishbase_traits <- function(fb) {
  get_mean_log <- function(x) {
    if (all(is.na(x))) return(NA)
    mean(log(x[x > 0]), na.rm = TRUE)
  }

  get_sd_log <- function(x) {
    if (all(is.na(x))) return(NA)
    sd(log(x[x > 0]), na.rm = TRUE) #/ sqrt(sum(!is.na(x[x > 0])))
  }

  loo <- fb$popgrowth$Loo
  k <- fb$popgrowth$K
  m <- fb$popgrowth$M
  lmax <- fb$popchar$Lmax
  tmax <- fb$popchar$tmax
  lm <- fb$maturity$Lm
  tm <- fb$maturity$tm
  fec <- fb$fecundity$FecundityMean

  traits <- tibble::tibble(
    trait = c("log(age_max)", "log(fecundity)", "log(growth_coefficient)",
              "log(length_max)", "log(length_infinity)", "log(length_maturity)",
              "log(age_maturity)", "log(natural_mortality)"),
    mean_normal = c(
      mean(tmax, na.rm=T),
      mean(fec, na.rm=T),
      mean(k, na.rm=T),
      mean(lmax, na.rm=T),
      mean(loo, na.rm=T),
      mean(lm, na.rm=T),
      mean(tm, na.rm=T),
      mean(m, na.rm=T)
    ),
    sd_normal = c(
      sd(tmax, na.rm=T),
      sd(fec, na.rm=T),
      sd(k, na.rm=T),
      sd(lmax, na.rm=T),
      sd(loo, na.rm=T),
      sd(lm, na.rm=T),
      sd(tm, na.rm=T),
      sd(m, na.rm=T)
    ),
    mean = c(
      get_mean_log(tmax),
      get_mean_log(fec),
      get_mean_log(k),
      get_mean_log(lmax),
      get_mean_log(loo),
      get_mean_log(lm),
      get_mean_log(tm),
      get_mean_log(m)
    ),
    sd = c(
      get_sd_log(tmax),
      get_sd_log(fec),
      get_sd_log(k),
      get_sd_log(lmax),
      get_sd_log(loo),
      get_sd_log(lm),
      get_sd_log(tm),
      get_sd_log(m)
    )
  )

  traits$species <- unique(c(fb$popchar$Species,
                             fb$popgrowth$Species,
                             fb$popchar$Species,
                             fb$maturity$Species,
                             fb$fecundity$Species))

  return(traits[, c("species", "trait", "mean_normal", "sd_normal", "mean", "sd")])
}

#' Summarize FishBase Trait Data Tables, Traits, and Names
#'
#' This function takes no input, but is just useful for identifying the source
#' of trait info from FishBase and the corresponding table names.
#' @export
map_trait_fb <- function() {
  traits <- tibble::tibble(
    trait = c(
      "log(age_max)",
      "log(fecundity)",
      "log(growth_coefficient)",
      "log(length_max)",
      "log(length_infinity)",
      "log(length_maturity)",
      "log(age_maturity)",
      "log(natural_mortality)"
    ),
    fb_table = c(
      "popchar",      # age_max
      "fecundity",    # fecundity
      "popgrowth",    # growth_coefficient (K)
      "popchar",      # length_max (Lmax)
      "popgrowth",    # length_infinity (Loo)
      "maturity",     # length_maturity (Lm)
      "maturity",     # age_maturity (tm)
      "popgrowth"     # natural_mortality (M)
    ),
    fb_trait = c(
      "tmax",         # age_max
      "FecundityMean",# fecundity
      "K",            # growth_coefficient
      "Lmax",         # length_max
      "Loo",          # length_infinity
      "Lm",           # length_maturity
      "tm",           # age_maturity
      "M"             # natural_mortality
    )
  )
  return(traits)
}


