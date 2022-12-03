#' Tidy eval helpers
#'
#' @description
#' This page lists the tidy eval tools reexported in this package from
#' rlang. To learn about using tidy eval in scripts and packages at a
#' high level, see the [dplyr programming
#' vignette](https://dplyr.tidyverse.org/articles/programming.html)
#' and the [ggplot2 in packages
#' vignette](https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html).
#' The [Metaprogramming
#' section](https://adv-r.hadley.nz/metaprogramming.html) of [Advanced
#' R](https://adv-r.hadley.nz) may also be useful for a deeper dive.
#'
#' * The `.data` pronoun is an object that represents the current
#'   slice of data. If you have a variable name in a string, use the
#'   `.data` pronoun to subset that variable with `[[`.
#'
#'   ```
#'   my_var <- "disp"
#'   mtcars |> summarise(mean = mean(.data[[my_var]]))
#'   ```
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang .data .env
#' @aliases .data .env
NULL
