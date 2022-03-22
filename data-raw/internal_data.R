## We have a couple simple parameters to set up, let's do that here.
approved_books <- c(
  "Practical Python Programming",
  "R for Data Science",
  "R Packages",
  "Mastering Shiny"
)

days <- c(
  "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"
)

time_slots <- data.frame(
  time_slot = c(
    "12 AM (midnight)",
    paste(
      c(1:11),
      "AM"
    ),
    "12 PM (noon)",
    paste(
      c(1:11),
      "PM"
    )
  )
)

# To avoid existing clubs & use the facilitator's information, we would need to
# deal with timezones better (the available slots would be in UTC, then we'd
# convert them per user). I'm not prepared to do that yet, so let's go without.

# running_book_clubs <- matrix(FALSE, nrow = 24, ncol = 7)

# rpkgs_slots <- choose_time("rpkgs") %>%
#   # Expand it back out to days and hours.
#   dplyr::transmute(
#     day = lubridate::wday(
#       .data$datetime_utc,
#       label = TRUE,
#       abbr = FALSE,
#       week_start = 1
#     ),
#     hour = lubridate::hour(.data$datetime_utc),
#     available = TRUE
#   ) %>%
#   dplyr::arrange(.data$day, .data$hour) %>%
#   tidyr::pivot_wider(
#     names_from = .data$day,
#     values_from = .data$available,
#     values_fill = FALSE
#   ) %>%
#   dplyr::arrange(.data$hour)

# The logic: rhandsontable needs a df of values to use to create the table.
# FALSE = unselected, TRUE = selected, NA = no checkbox. So our starting point
# is a table full of falses for every timeslot, and then we'll fill in NAs
# whenever that slot isn't available (to this club). We will likely move all of
# that logic into a function that runs when the user loads the page, since it
# can change.
week_calendar <- data.frame(
  matrix(
    F,
    nrow = 24,
    ncol = 7,
    dimnames = list(time_slots$time_slot, days)
  )
)

usethis::use_data(
  approved_books,
  week_calendar,
  internal = TRUE,
  overwrite = TRUE
)

rm(
  time_slots,
  week_calendar,
  approved_books,
  days
)