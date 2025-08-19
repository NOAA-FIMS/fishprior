# transform_data_frame ----
## Setup ----
# Load or prepare any necessary data for testing

## IO correctness ----
test_that("transform_data_frame() works with correct inputs", {
  transformed <- transform_data_frame(traits_example)
  #' @description Test that transform_data_frame(x) returns a tibble.
  expect_equal(
    object = class(transformed),
    expected = c("tbl_df", "tbl", "data.frame")
  )

  #' @description Test that transform_data_frame(x) returns the correct column
  #' names.
  expect_equal(
    object = colnames(transformed),
    expected = c("Species", "trait", "mean_normal", "sd_normal", "mean", "se")
  )
})

## Edge handling ----
test_that("transform_data_frame() returns correct outputs for edge cases", {
  #' @description Test that transform_data_frame(x) does not error if given
  #' more than one group.
  expect_no_error(
    object = transform_data_frame(traits_example)
  )
})

## Error handling ----
test_that("transform_data_frame() returns correct error messages", {
  #' @description Test that transform_data_frame(x) errors if passed a vector.
  expect_error(
    object = transform_data_frame(1:10),
    regexp = "expects a data frame"
  )
})
