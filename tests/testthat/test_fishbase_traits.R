# get_fishbase_traits ----
## Setup ----
# Load or prepare any necessary data for testing

## IO correctness ----
test_that("get_fishbase_traits() works with correct inputs", {
  traits <- get_fishbase_traits(spec_names = "Gadus morhua")

  #' @description Test that get_fishbase_traits(x) returns a tibble.
  expect_equal(
    object = class(traits),
    expected = c("tbl_df", "tbl", "data.frame")
  )

  #' @description Test that get_fishbase_traits(x) returns the correct column
  #' names.
  expect_equal(
    object = colnames(traits),
    expected = c(
      "rfishbase", "Species", "SpecCode", "Sex", "PopGrowthRef",
      "DataSourceRef", "Locality", "YearStart", "YearEnd", "Number",
      "Type", "C_Code", "E_CODE", "SourceRef",
      "StockCode", "AgeMatRef", "trait", "value", "SE",
      "SD", "country", "EcosystemName"
    )
  )

  #' @description Test that get_fishbase_traits(x) returns lots of rows
  expect_gt(nrow(traits), 600)

  #' @description Test that get_fishbase_traits(x) returns data from correct
  #' tables.
  expect_equal(unique(traits$rfishbase), expected = c(
    "popgrowth",
    "popchar",
    "poplw",
    "maturity",
    "fecundity"
  ))
})
