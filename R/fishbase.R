#' Retrieve FishBase life-history trait data for species
#'
#' Queries FishBase via the \pkg{rfishbase} package and returns raw data tables
#' containing life-history traits relevant for meta-analysis or prior
#' construction.
#'
#' @param spec_names A character vector of species names (e.g., `"Gadus
#' morhua"`).
#'
#' @return
#' A tibble with metadata and trait information. The tibble contains the
#' following columns:
#' \describe{
#'   \item{`rfishbase`}{The corresponding rfishbase function used to get the
#'     trait information, e.g., [rfishbase::popgrowth()].}
#'   \item{Species}{The scientific name of the species.}
#'   \item{SpecCode}{The unique species code used in FishBase.}
#'   \item{Sex}{A lower-case string indicating the sex of the specimens
#'     included in the study. Note that this column was converted to lower case
#'     for consistency.}
#'   \item{PopGrowthRef}{}
#'   \item{DataSourceRef}{}
#'   \item{Locality}{The locality where the data was collected.}
#'   \item{YearStart}{The first year of data collection.}
#'   \item{YearEnd}{The last year of data collection.}
#'   \item{Number}{The number of individuals used to estimate the parameters.}
#'   \item{Type}{The type of measurements used. For example, when measuring
#'     length for the growth model, the type of length measurement will be
#'     listed (e.g., "TL" = total length, "SL" = standard length, etc.). This
#'     column is important for consistency with [rfishbase::poplw()] or
#'     [rfishbase::popchar()].}
#'   \item{SourceRef}{}
#'   \item{StockCode}{}.
#'   \item{AgeMatRef}{}.
#'   \item{trait}{The name of the trait taken from FishBase.}
#'   \item{value}{The value of the measured or calculated trait.}
#'   \item{SE}{The standard error of the trait value. This information is not
#'     available for all traits, and will be `NA` if not available.}
#'   \item{SD}{The standard deviation of the trait value. This information is
#'     not available for all traits, and will be `NA` if not available.}
#' }
#' @details
#' There are many traits available in FishBase but this function only returns
#' those that were relevant for assessment and life-history modeling. Should
#' you wish that more traits be included, please open an [issue on GitHub](
#' https://github.com/NOAA-FIMS/fishprior/issues). Below is a list of the
#' traits that are available in the `trait` column:
#' \describe{
#'   \item{Loo}{Asymptotic length (L∞) in the von Bertalanffy Growth Function
#'     (typically in cm).}
#'   \item{K}{Growth coefficient (K) from the von Bertalanffy model (in
#'     1/year).}
#'   \item{to}{Theoretical age at zero length (t₀) from the von Bertalanffy
#'     model (in years).}
#'   \item{M}{Natural mortality rate (M) estimated from the growth model or
#'     accompanying source.}
#'   \item{Wmax}{The maximum observed body weight in the population (usually in
#'     grams).}
#'   \item{Lmax}{The maximum observed length in the population (usually in cm).}
#'   \item{tmax}{The maximum observed age (in years).}
#'   \item{LengthMax}{The maximum length observed in the dataset used to fit
#'     the length--weight relationship (same units as Type, usually cm).}
#'   \item{a}{This parameter can have multiple meanings depending on the
#'     function that was used to estimate it, see the `rfishbase` column for
#'     context. If the function was [rfishbase::poplw()], then the a parameter
#'     is the intercept of the length--weight relationship, i.e., W = a * L^b.
#'     If the function was [rfishbase::fecundity()], then a is the intercept of
#'     the fecundity relationship.}
#'   \item{b}{This parameter can have multiple meanings depending on the
#'     function that was used to estimate it, see the `rfishbase` column for
#'     context. If the function was [rfishbase::poplw()], then b is the slope
#'     of the length--weight relationship, i.e., W = a * L^b. This is often
#'     approximately 3 for isometric growth. If the function was
#'     [rfishbase::fecundity()], then b is the slope of the fecundity
#'     relationship.}
#'   \item{tm}{The age at maturity (in years), interpreted as the age when 50%
#'     are mature.}
#'   \item{Lm}{The length at maturity (typically in cm), often L50 (length at
#'     50% maturity).}
#'   \item{FecundityMin}{The minimum fecundity observed in the population.}
#'   \item{FecundityMax}{The maximum fecundity observed in the population.}
#'   \item{FecundityMean}{The mean fecundity observed in the population.}
#' }
#'
#' @export
get_fishbase_traits <- function(spec_names = NULL) {
  growth <- rfishbase::popgrowth(species_list = spec_names) |>
    dplyr::select(
      Species, SpecCode, Sex, PopGrowthRef, DataSourceRef, Locality,
      YearStart, YearEnd, Number, Type,
      Loo, SE_Loo, SD_Loo,
      K, SE_K, SD_K,
      to, SE_to, SD_to,
      M, SE_M, SD_M
    )

  char <- rfishbase::popchar(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, Sex, SourceRef,
                  Wmax, Lmax, tmax, Locality) |>
    dplyr::mutate(
      Lmax = as.numeric(Lmax),
    )

  # lf <- rfishbase::poplf(species_list = spec_names) |>
  #   dplyr::select(Species, SpecCode, StockCode,
  #                 Locality, Sex)

  lw <- rfishbase::poplw(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode,
                  LengthMax, Type, Number, Sex,
                  a, b, SEa, SEb, SDa, SDb, Locality)

  mat <- rfishbase::maturity(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode, Sex, Locality,
                  AgeMatRef, tm, Number, SE_tm, SD_tm,
                  Lm, SD_Lm, SE_Lm
    ) |>
    dplyr::mutate(
      StockCode = as.numeric(StockCode)
    )

  fecund <- rfishbase::fecundity(species_list = spec_names) |>
    dplyr::select(Species, SpecCode, StockCode, Locality,
                  FecundityMin, FecundityMax, FecundityMean,
                  FecundityType, Number, a, b, SEa, SEb,
                  SDa, SDb) |>
    dplyr::mutate(
      SDa = ifelse(is.na(SDa), NA_real_, SDa),
      SDb = ifelse(is.na(SDb), NA_real_, SDb),
      FecundityMin = as.numeric(FecundityMin),
      FecundityMax = as.numeric(FecundityMax),
      FecundityMean = as.numeric(FecundityMean)
    ) |>
    dplyr::rename(
      Type = FecundityType
    )

  # The following doesn't work because of a mismatch in classes being returned
  # between the different rfishbase functions.
  # results <- purrr::map(
  #   list(
  #     rfishbase::popgrowth, rfishbase::popchar, rfishbase::poplw,
  #     rfishbase::maturity, rfishbase::fecundity
  #   ),
  #   ~ .x(species_list = spec_names)
  # ) |>
  #   dplyr::bind_rows()

  list(
    popgrowth = growth,
    popchar = char,
    poplw = lw,
    maturity = mat,
    fecundity = fecund
  ) |>
    dplyr::bind_rows(.id = "rfishbase") |>
    dplyr::mutate(
      Sex = tolower(Sex)
    ) |>
    dplyr::rename_with(
      .f = \(x) paste0(x, "_value"),
      .cols = c(
        "Loo", "K", "to", "M", "a", "b", "Lm", "tm",
      )
    ) |>
    dplyr::rename_with(
      .f = \(x) gsub(
        pattern = "(.+)([mM][aei][a-zA-Z]{1,2}$)",
        # perl = TRUE,
        # replacement = "\\1_\\L\\2",
        replacement = "\\1\\2_value",
        x = x
      ),
      .cols = dplyr::everything()
    ) |>
    dplyr::rename_with(
      .f = \(x) gsub(
        pattern = "^(S[D|E])_{0,1}(.+)",
        replacement = "\\2_\\1",
        x = x
      ),
      .cols = dplyr::everything()
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("_"),
      names_to = c("trait", ".value"),
      names_sep = "_",
      values_drop_na = TRUE
    )
}

#' Summarize FishBase trait data into log-transformed traits
#'
#' Takes the output of [get_fishbase_traits()] and summarizes it into
#' log-transformed life-history traits. The returned tibble includes species,
#' trait name, mean (on log-scale), and approximate standard error (SE).
#'
#' @param fb A tibble returned by [get_fishbase_traits()].
#'
#' @return A tibble that is grouped by Species with the following columns:
#' \describe{
#'   \item{`Species`}{Species name.}
#'   \item{`trait`}{Name of the trait (e.g., `log(length_infinity)`).}
#'   \item{`mean_normal`}{Mean of the trait in normal space}
#'   \item{`sd_normal`}{Approximate standard deviation of the trait in normal space.}
#'   \item{`mean`}{Mean of the log-transformed trait}
#'   \item{`sd`}{Approximate standard deviation of the log-transformed trait.}
#' }
#'
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

  fb |> dplyr::filter(
    trait %in% c(
      "Loo", "K", "M", "Lmax", "tmax", "Lm", "tm", "FecundityMean"
    )
  ) |>
    dplyr::group_by(Species, trait) |>
    dplyr::summarise(
      mean_normal = mean(value, na.rm = TRUE),
      sd_normal = sd(value, na.rm = TRUE),
      mean = get_mean_log(value),
      sd = get_sd_log(value)
    ) |>
    dplyr::mutate(
      trait = translate_trait_names(trait)
    )
}
