#' Retrieve FishBase life-history trait data for species
#'
#' Queries FishBase via the \pkg{rfishbase} package and returns raw data tables
#' containing life-history traits relevant for meta-analysis or prior construction.
#' @param spec_names A character vector of species names (e.g., "Gadus morhua").
#' @importFrom rlang .data
#' @return A tibble with metadata and trait information.
#' @export
get_fishbase_traits <- function(spec_names = NULL) {
  growth <- rfishbase::popgrowth(species_list = spec_names) |>
    dplyr::select(
      "Species", "SpecCode", "Sex", "PopGrowthRef",
      "DataSourceRef", "Locality", "YearStart", "YearEnd",
      "Number", "Type",
      "Loo", "SE_Loo", "SD_Loo",
      "K", "SE_K", "SD_K",
      "to", "SE_to", "SD_to",
      "M", "SE_M", "SD_M",
      "Winfinity", "C_Code", "E_CODE"
    ) |>
    dplyr::mutate(
      C_Code = as.character(.data$C_Code),
      E_CODE = as.numeric(.data$E_CODE)
    )

  char <- rfishbase::popchar(species_list = spec_names) |>
    dplyr::select(
      "Species", "SpecCode", "Sex", "SourceRef",
      "Wmax", "Lmax", "tmax", "Locality", "C_Code"
    ) |>
    dplyr::mutate(
      Lmax = as.numeric(.data$Lmax),
      C_Code = as.character(.data$C_Code)
    )

  lw <- rfishbase::poplw(species_list = spec_names) |>
    dplyr::select(
      "Species", "SpecCode", "StockCode",
      "LengthMax", "Type", "Number", "Sex",
      "a", "b", "SEa", "SEb", "SDa", "SDb",
      "Locality", "C_Code"
    ) |>
    dplyr::mutate(
      C_Code = as.character(.data$C_Code)
    )

  mat <- rfishbase::maturity(species_list = spec_names) |>
    dplyr::select(
      "Species", "SpecCode", "StockCode", "Sex",
      "Locality", "AgeMatRef", "tm", "Number",
      "SE_tm", "SD_tm", "Lm", "SD_Lm", "SE_Lm",
      "C_Code", "E_CODE"
    ) |>
    dplyr::mutate(
      StockCode = as.numeric(.data$StockCode),
      C_Code = as.character(.data$C_Code),
      E_CODE = as.numeric(.data$E_CODE)
    )

  fecund <- rfishbase::fecundity(species_list = spec_names) |>
    dplyr::select(
      "Species", "SpecCode", "StockCode", "Locality",
      "FecundityMin", "FecundityMax", "FecundityMean",
      "FecundityType", "Number", "a", "b",
      "SEa", "SEb", "SDa", "SDb", "C_Code", "E_CODE"
    ) |>
    dplyr::mutate(
      SDa = ifelse(is.na(.data$SDa), NA_real_, .data$SDa),
      SDb = ifelse(is.na(.data$SDb), NA_real_, .data$SDb),
      FecundityMin = as.numeric(.data$FecundityMin),
      FecundityMax = as.numeric(.data$FecundityMax),
      FecundityMean = as.numeric(.data$FecundityMean),
      C_Code = as.character(.data$C_Code),
      E_CODE = as.numeric(.data$E_CODE)
    ) |>
    dplyr::rename(Type = FecundityType)

  traits <- list(
    popgrowth = growth,
    popchar = char,
    poplw = lw,
    maturity = mat,
    fecundity = fecund
  ) |>
    dplyr::bind_rows(.id = "rfishbase") |>
    dplyr::mutate(
      Sex = tolower(.data$Sex)
    ) |>
    dplyr::rename_with(
      .fn = \(x) paste0(x, "_value"),
      .cols = c("Loo", "K", "to", "M", "a", "b", "Lm", "tm", "Winfinity")
    ) |>
    dplyr::rename_with(
      .fn = \(x) gsub(pattern = "(.+)([mM][aei][a-zA-Z]{1,2}$)", replacement = "\\1\\2_value", x = x),
      .cols = dplyr::everything()
    ) |>
    dplyr::rename_with(
      .fn = \(x) gsub(pattern = "^(S[D|E])_{0,1}(.+)", replacement = "\\2_\\1", x = x),
      .cols = dplyr::everything()
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("(_value$|_SE$|_SD$)"),
      names_to = c("trait", ".value"),
      names_sep = "_",
      values_drop_na = TRUE
    ) |>
    dplyr::group_by(.data$Locality) |>
    dplyr::mutate(
      C_Code = dplyr::first(stats::na.omit(.data$C_Code))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$C_Code) |>
    dplyr::mutate(
      E_CODE = dplyr::first(stats::na.omit(.data$E_CODE))
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      get_fishbase_country_names(),
      by = c("C_Code" = "C_Code")
    ) |>
    dplyr::left_join(
      get_fishbase_ecosystem_names(spec_names),
      by = c("E_CODE" = "E_CODE")
    ) |>
    dplyr::tibble()

  return(traits)
}


#' Summarize FishBase trait data into log-transformed traits
#'
#' @param fb A tibble returned by [get_fishbase_traits()].
#' @importFrom rlang .data
#' @return A tibble grouped by Species and trait with log-transformed stats.
#' @export
summarize_fishbase_traits <- function(fb) {
  get_mean_log <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    mean(log(x[x > 0]), na.rm = TRUE)
  }

  get_sd_log <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    stats::sd(log(x[x > 0]), na.rm = TRUE)
  }

  fb |>
    dplyr::filter(.data$trait %in% c("Loo", "K", "M", "Lmax", "tmax", "Lm", "tm", "FecundityMean")) |>
    dplyr::group_by(.data$Species, .data$trait) |>
    dplyr::summarise(
      mean_normal = mean(.data$value, na.rm = TRUE),
      sd_normal = stats::sd(.data$value, na.rm = TRUE),
      mean = get_mean_log(.data$value),
      sd = get_sd_log(.data$value),
      .groups = "drop"
    ) |>
    dplyr::mutate(trait = translate_trait_names(.data$trait))
}

#' Retrieve FishBase country or ecosystem names
#'
#' @return A tibble of country or ecosystem code-to-name mappings
#' @importFrom rlang .data
#' @noRd
get_fishbase_country_names <- function() {
  rfishbase::c_code() |>
    dplyr::distinct(.data$C_Code, .data$country) |>
    dplyr::mutate(C_Code = as.character(.data$C_Code)) |>
    as.data.frame()
}

#' @importFrom rlang .data
get_fishbase_ecosystem_names <- function(species = NULL) {
  rfishbase::ecosystem(species = species) |>
    dplyr::distinct(.data$E_CODE, .data$EcosystemName) |>
    dplyr::mutate(E_CODE = as.integer(.data$E_CODE)) |>
    as.data.frame()
}
