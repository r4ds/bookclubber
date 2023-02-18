## We have a couple simple parameters to set up, let's do that here.
.gs4_sheet_id <- "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc"

.days <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
)
.days_factor <- factor(.days, levels = .days, ordered = TRUE)

.time_slots <- c(
  "12:00 AM",
  paste0(
    c(1:11),
    ":00 AM"
  ),
  "12:00 PM",
  paste0(
    c(1:11),
    ":00 PM"
  )
)

.week_calendar_long <- dplyr::tibble(
  day = rep(.days_factor, each = 24),
  hour = rep(0:23, 7)
)

.team_id <- "T6UC1DKJQ"

usethis::use_data(
  .gs4_sheet_id,
  .week_calendar_long,
  .team_id,
  internal = TRUE,
  overwrite = TRUE
)

rm(
  .week_calendar_long,
  .gs4_sheet_id,
  .days,
  .days_factor,
  .team_id
)
