#' Transform log mean and standard error to normal space
#'
#' @param data A data frame returned from [get_fishlife_traits()] with the
#'   following columns: `name`, `trait`, `mean`, and `se`. The `mean` and
#'   `se` columns should be in log space.
#' @return
#' The original data frame is returned with two additional columns,
#' `mean_normal` and `sd_normal`.
transform_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.fn transform_data_frame} expects a data frame.",
      "i" = "Please provide a data frame with the following columns:
      {.val name}, {.val trait}, {.val mean}, and {.val se}."
    ))
  }
  var_log <- data[["se"]]^2
  # Calculate mean in normal space
  data[["mean_normal"]] <- exp(data[["mean"]] + 0.5 * var_log)
  # Calculate the variance in normal space
  var_normal <- (exp(var_log) - 1) * exp(2 * data[["mean"]] + var_log)
  # Convert to SE in normal space
  data[["sd_normal"]] <- sqrt(var_normal)
  return(tibble::as_tibble(data))
}
