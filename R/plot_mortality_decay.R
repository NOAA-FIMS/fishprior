#' Plot the decay of natural mortality by age
#'
#' Starting at age zero to the maximum theoretical age (based on natural
#' mortality), the decay of mortality (y axis) is shown by age (years).
#' Lines are colored by group, where groups are defined by the names of
#' the input vector.
#'
#' @param x A vector of positive real numbers specifying natural mortality
#'   for each desired group. The vector should be a named vector if it has a
#'   length of great than one, e.g., `c("female" = 0.33, "male" = 0.2)`.
#'
#' @return
#' A {ggplot2} object is returned.
#' @export
#' @seealso
#' * `calc_max_age()` for how the maximum age is calculated
#' @examples
#' plot_mortality_decay(0.2)
#' plot_mortality_decay(seq(0.2, 0.4, by = 0.1))
#' plot_mortality_decay(c("female" = .2, "male" = .4))
plot_mortality_decay <- function(x) {
  if (is.null(names(x))) {
    names(x) <- glue::glue("Group {seq(x)}"
    )
  }
  internal_data <- purrr::map(
    x,
    .f = \(y) data.frame(
      max_age = calc_max_age(y),
      Ages = 0:calc_max_age(y),
      PopN = exp(-y * 0:calc_max_age(y))
    )
  ) |>
    dplyr::bind_rows(.id = "Max age") |>
    dplyr::mutate(`Max age` = glue::glue("{`Max age`} ({max_age})"))

  ggplot2::ggplot(
    internal_data,
    ggplot2::aes(Ages, PopN, color = `Max age`)
  ) +
  ggplot2::geom_line(ggplot2::aes(linetype = `Max age`), lwd = 2) +
  ggplot2::ylab("Cohort decline by natural mortality")
}
