#' Calculate maximum age from natural mortality
#'
#' Using Hamel and Cope (2022), the maximum observed age can serve as a proxy
#' for longevity of marine fishes where natural mortality equals 5.4 divided by
#' the maximum age. This function inverts that formula to determine the maximum
#' age that you should see in your population given the estimate of natural
#' mortality.
#'
#' @param natural_mortality A vector of positive real numbers specifying the
#'   natural mortality for a given group.
#' @return
#' A vector of estimates of maximum age, where the length and names of the
#' returned vector are determined by the input vector.
#' @export
calc_max_age <- function(natural_mortality) {
  if (!is.numeric(natural_mortality) || is.complex(natural_mortality)) {
    cli::cli_abort("{.arg natural_mortality} must be numeric")
  }
  if (!is.null(dim(natural_mortality))) {
    cli::cli_abort("{.arg natural_mortality} must be a vector")
  }
  if (length(natural_mortality) == 0) {
    cli::cli_abort("{.arg natural_mortality} must not be empty")
  }
  if (any(!is.finite(natural_mortality))) {
    cli::cli_abort("{.arg natural_mortality} must contain only finite values")
  }
  if (any(natural_mortality <= 0)) {
    cli::cli_abort("{.arg natural_mortality} must be greater than 0")
  }

  # natural mortality = 5.4 / maximum age
  5.4 / natural_mortality
}
