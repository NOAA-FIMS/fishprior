# summarize_fishbase_traits() takes the long-format tibble from
# get_fishbase_traits() and returns one row per (Species, trait) with
# log-scale summary statistics.  We build synthetic input that mirrors
# the minimal columns the function actually touches.

make_fb_data <- function() {
  tibble::tibble(
    Species = c(
      rep("Gadus morhua",      6),
      rep("Anoplopoma fimbria", 4)
    ),
    trait = c(
      # Gadus: three traits
      "Loo", "Loo", "K", "K", "M", "M",
      # Anoplopoma: two traits
      "Loo", "K", "tmax", "FecundityMean"
    ),
    value = c(
      # Gadus
      120, 115,           # Loo
      0.20, 0.23,         # K
      0.30, 0.28,         # M
      # Anoplopoma
      90,                 # Loo
      0.10,               # K
      80,                 # tmax
      500000             # FecundityMean
    )
  )
}

test_that("summarize_fishbase_traits() returns a data frame", {
  result <- summarize_fishbase_traits(make_fb_data())
  expect_s3_class(result, "data.frame")
})

test_that("summarize_fishbase_traits() has the expected columns", {
  result <- summarize_fishbase_traits(make_fb_data())
  expect_true(all(c("Species", "trait", "mean_normal", "sd_normal", "mean", "sd")
                  %in% colnames(result)))
})

test_that("summarize_fishbase_traits() has one row per Species-trait combination", {
  data   <- make_fb_data()
  result <- summarize_fishbase_traits(data)

  expected_rows <- data |>
    dplyr::filter(trait %in% c("Loo", "K", "M", "Lmax", "tmax", "Lm", "tm",
                                "FecundityMean")) |>
    dplyr::distinct(Species, trait) |>
    nrow()

  expect_equal(nrow(result), expected_rows)
})

test_that("summarize_fishbase_traits() only keeps the eight supported traits", {
  # Add a trait that should be dropped
  data <- tibble::add_row(make_fb_data(), Species = "Gadus morhua",
                           trait = "Winfinity", value = 20000)

  result <- summarize_fishbase_traits(data)
  supported <- c(
    "log(length_infinity)", "log(growth_coefficient)", "log(natural_mortality)",
    "log(length_max)",      "log(age_max)",            "log(length_maturity)",
    "log(age_maturity)",    "log(fecundity)"
  )
  expect_true(all(result[["trait"]] %in% supported))
})

test_that("summarize_fishbase_traits() computes log-scale mean correctly", {
  # Single species, single trait — easy to verify by hand
  data <- tibble::tibble(
    Species = rep("TestSpecies", 3),
    trait   = rep("Loo", 3),
    value   = c(100, 110, 120)
  )
  result <- summarize_fishbase_traits(data)

  expected_mean_log <- mean(log(c(100, 110, 120)))
  expect_equal(result[["mean"]], expected_mean_log, tolerance = 1e-10)
})

test_that("summarize_fishbase_traits() computes log-scale sd correctly", {
  data <- tibble::tibble(
    Species = rep("TestSpecies", 3),
    trait   = rep("K", 3),
    value   = c(0.1, 0.2, 0.3)
  )
  result <- summarize_fishbase_traits(data)

  expected_sd_log <- sd(log(c(0.1, 0.2, 0.3)))
  expect_equal(result[["sd"]], expected_sd_log, tolerance = 1e-10)
})

test_that("summarize_fishbase_traits() computes normal-scale mean and sd correctly", {
  values <- c(100, 110, 120)
  data   <- tibble::tibble(
    Species = rep("TestSpecies", 3),
    trait   = rep("Loo", 3),
    value   = values
  )
  result <- summarize_fishbase_traits(data)

  expect_equal(result[["mean_normal"]], mean(values),   tolerance = 1e-10)
  expect_equal(result[["sd_normal"]],   sd(values),     tolerance = 1e-10)
})

test_that("summarize_fishbase_traits() translates trait names to log-scale snake_case", {
  data <- tibble::tibble(
    Species = rep("TestSpecies", 2),
    trait   = c("M", "Loo"),
    value   = c(0.3, 100)
  )
  result <- summarize_fishbase_traits(data)

  expect_true("log(natural_mortality)"  %in% result[["trait"]])
  expect_true("log(length_infinity)"   %in% result[["trait"]])
})

test_that("summarize_fishbase_traits() handles a single observation per trait", {
  data <- tibble::tibble(
    Species = "SingleObs",
    trait   = "M",
    value   = 0.25
  )
  result <- summarize_fishbase_traits(data)

  expect_equal(nrow(result), 1)
  expect_equal(result[["mean_normal"]], 0.25)
  # sd of a single value is NA
  expect_true(is.na(result[["sd_normal"]]))
  expect_equal(result[["mean"]], log(0.25), tolerance = 1e-10)
})

test_that("summarize_fishbase_traits() returns zero rows for unsupported traits only", {
  data <- tibble::tibble(
    Species = "TestSpecies",
    trait   = "Winfinity",
    value   = 1000
  )
  result <- summarize_fishbase_traits(data)
  expect_equal(nrow(result), 0)
})
