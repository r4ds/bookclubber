## We have a couple simple parameters to set up, let's do that here.
.gs4_sheet_id <- "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc"

.days <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
)

.time_slots <- c(
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

# The logic: rhandsontable needs a df of values to use to create the table.
# FALSE = unselected, TRUE = selected, NA = no checkbox. So our starting point
# is a table full of falses for every timeslot, and then we'll fill in NAs
# whenever that slot isn't available (to this club). We will likely move all of
# that logic into a function that runs when the user loads the page, since it
# can change.
.week_calendar <- data.frame(
  matrix(
    F,
    nrow = length(.time_slots),
    ncol = length(.days),
    dimnames = list(.time_slots, .days)
  )
)

.team_id <- "T6UC1DKJQ"
.site_url <- "https://r4dscommunity.shinyapps.io/bookclubber/"

usethis::use_data(
  .gs4_sheet_id,
  .week_calendar,
  .team_id,
  .site_url,
  internal = TRUE,
  overwrite = TRUE
)

rm(
  .time_slots,
  .week_calendar,
  .gs4_sheet_id,
  .days,
  .team_id,
  .site_url
)
