## We have a couple simple parameters to set up, let's do that here.

approved_books <- c("mshiny", "rpkgs")

days <- c(
  "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"
)

time_slots <- data.frame(
  time_slot = c(
    paste(
      c(12, 1:11),
      "AM"
    ),
    paste(
      c(12, 1:11),
      "PM"
    )
  )
)

running_book_clubs <- matrix(FALSE, nrow = 24, ncol = 7)

# This requires better timezone handling to make sense, so for now I'm skipping
# the running_book_clubs stuff.

week_calendar <- (
  running_book_clubs -
    matrix(
      F,
      nrow = 24,
      ncol = 7,
      dimnames = list(time_slots$time_slot, days)
    )
)  %>%
  data.frame() %>%
  dplyr::mutate(
    dplyr::across(
      c(.data$Monday:.data$Sunday),
      dplyr::na_if,
      TRUE
    )
  ) %>%
  dplyr::mutate_at(
    dplyr::vars(.data$Monday:.data$Sunday),
    as.logical
  )

usethis::use_data(
  approved_books,
  week_calendar,
  internal = TRUE,
  overwrite = TRUE
)
