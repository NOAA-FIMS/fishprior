
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
      "rfishbase", "SpecCode", "Sex", "PopGrowthRef",
      "DataSourceRef", "Locality", "YearStart", "YearEnd", "Number",
      "Type", "C_Code", "E_CODE", "SourceRef",
      "StockCode", "AgeMatRef", "trait", "value", "SE",
      "SD", "country", "EcosystemName", "Species"
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

# get_fishbase_traits() makes 8 rfishbase network calls. We mock every one of
# them so the full pipeline is exercised without any network dependency.
#
# Mock column types are chosen to match what rfishbase actually returns so
# the mutate() coercions inside get_fishbase_traits() are realistic.

# Mock data factories
mock_popgrowth <- function(...) {
  tibble::tibble(
    SpecCode      = 1L,
    Sex           = "Female",
    PopGrowthRef  = 101L,
    DataSourceRef = 201L,
    Locality      = "North Sea",
    YearStart     = 1990L,
    YearEnd       = 2000L,
    Number        = 50L,
    Type          = "A",
    Loo           = 120.0,
    SE_Loo        = 5.0,
    SD_Loo        = 4.0,
    K             = 0.20,
    SE_K          = 0.01,
    SD_K          = 0.01,
    to            = -0.5,
    SE_to         = 0.10,
    SD_to         = 0.10,
    M             = 0.30,
    SE_M          = 0.05,
    SD_M          = 0.04,
    Winfinity     = 15000.0,
    C_Code        = "826",
    E_CODE        = 1.0
  )
}

mock_popchar <- function(...) {
  tibble::tibble(
    SpecCode  = 1L,
    Sex       = "Female",
    SourceRef = 301L,
    Wmax      = 20000.0,
    Lmax      = "130",     # rfishbase returns Lmax as character
    tmax      = 25.0,
    Locality  = "North Sea",
    C_Code    = "826"
  )
}

mock_poplw <- function(...) {
  tibble::tibble(
    SpecCode  = 1L,
    StockCode = 1L,
    LengthMax = 130.0,
    Type      = "TL",
    Number    = 100L,
    Sex       = "unsexed",
    a         = 0.008,
    b         = 3.03,
    SEa       = 0.001,
    SEb       = 0.020,
    SDa       = 0.001,
    SDb       = 0.020,
    Locality  = "North Sea",
    C_Code    = "826"
  )
}

mock_maturity <- function(...) {
  tibble::tibble(
    SpecCode  = 1L,
    StockCode = "1",       # rfishbase returns StockCode as character
    Sex       = "Female",
    Locality  = "North Sea",
    AgeMatRef = 401L,
    tm        = 3.0,
    Number    = 30L,
    SE_tm     = 0.20,
    SD_tm     = 0.20,
    Lm        = 50.0,
    SD_Lm     = 3.0,
    SE_Lm     = 3.0,
    C_Code    = "826",
    E_CODE    = 1.0
  )
}

mock_fecundity <- function(...) {
  tibble::tibble(
    SpecCode      = 1L,
    StockCode     = 1L,
    Locality      = "North Sea",
    FecundityMin  = "100000",   # rfishbase returns these as character
    FecundityMax  = "500000",
    FecundityMean = "300000",
    FecundityType = "batch",
    Number        = 20L,
    a             = 0.50,
    b             = 2.00,
    SEa           = 0.05,
    SEb           = 0.10,
    SDa           = NA_real_,
    SDb           = NA_real_,
    C_Code        = "826",
    E_CODE        = 1.0
  )
}

mock_species <- function(...) {
  tibble::tibble(SpecCode = 1L, FBname = "Atlantic cod")
}

mock_c_code <- function(...) {
  tibble::tibble(C_Code = "826", country = "United Kingdom")
}

mock_ecosystem <- function(...) {
  tibble::tibble(E_CODE = 1L, EcosystemName = "North Sea")
}

# Shared mock setup
# Applies all rfishbase mocks and silences check_rfishbase_version() for the
# duration of the calling test_that() block.
setup_fishbase_mocks <- function() {
  testthat::local_mocked_bindings(
    popgrowth = mock_popgrowth,
    popchar   = mock_popchar,
    poplw     = mock_poplw,
    maturity  = mock_maturity,
    fecundity = mock_fecundity,
    species   = mock_species,
    c_code    = mock_c_code,
    ecosystem = mock_ecosystem,
    .package  = "rfishbase"
  )
  testthat::local_mocked_bindings(
    check_rfishbase_version = function() invisible(TRUE)
  )
}

# get_fishbase_traits() 
test_that("get_fishbase_traits() returns a tibble", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
})

test_that("get_fishbase_traits() contains the rfishbase source column", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("rfishbase" %in% colnames(result))
  expect_true(all(result[["rfishbase"]] %in%
                    c("popgrowth", "popchar", "poplw", "maturity", "fecundity")))
})

test_that("get_fishbase_traits() contains a trait column with known traits", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("trait" %in% colnames(result))
  expect_true(any(result[["trait"]] %in% c("Loo", "K", "M", "Lm", "tm")))
})

test_that("get_fishbase_traits() contains a value column", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("value" %in% colnames(result))
  expect_true(all(result[["value"]] > 0, na.rm = TRUE))
})

test_that("get_fishbase_traits() joins country names", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("country" %in% colnames(result))
  expect_true(any(result[["country"]] == "United Kingdom", na.rm = TRUE))
})

test_that("get_fishbase_traits() joins ecosystem names", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("EcosystemName" %in% colnames(result))
})

test_that("get_fishbase_traits() joins species common name", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true("Species" %in% colnames(result))
  expect_true(any(result[["Species"]] == "Atlantic cod", na.rm = TRUE))
})

test_that("get_fishbase_traits() lowercases Sex column", {
  setup_fishbase_mocks()
  result <- get_fishbase_traits("Gadus morhua")
  expect_true(all(result[["Sex"]] == tolower(result[["Sex"]]), na.rm = TRUE))
})

test_that("get_fishbase_traits() works with NULL spec_names", {
  setup_fishbase_mocks()
  expect_no_error(get_fishbase_traits(NULL))
})

# The get_mean_log() and get_sd_log() helpers have an early return for all-NA
# input. values_drop_na = TRUE in pivot_longer prevents this in normal use, so
# we hit it by calling summarize_fishbase_traits() directly with NA values.
test_that("summarize_fishbase_traits() handles all-NA values gracefully", {
  data <- tibble::tibble(
    Species = c("TestSpecies", "TestSpecies"),
    trait   = c("Loo", "Loo"),
    value   = c(NA_real_, NA_real_)
  )
  result <- summarize_fishbase_traits(data)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result[["mean"]]))
  expect_true(is.na(result[["sd"]]))
})