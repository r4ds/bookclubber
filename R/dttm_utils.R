#' Make a UTC time vector
#'
#' @param day A vector of days.
#' @param hour A vector of hours.
#' @param timezone A vector of starting timezones.
#'
#' @return A vector of equivalent UTC times "next week."
#' @keywords internal
.make_utc <- function(day, hour, timezone) {
  # I haven't sorted out how to vectorize most of this so I need to repeat some
  # calculations. And ugh that unlist but then re-datetime is dumb but I haven't
  # found a better way.
  return(
    lubridate::as_datetime(
      unlist(
        purrr::pmap(
          list(day = day, hour = hour, timezone = timezone),
          .make_single_utc
        )
      )
    )
  )
}

#' Make a single UTC time
#'
#' @param day A single day.
#' @param hour A single hour.
#' @param timezone A single timezone.
#'
#' @return A single UTC time, equivalent to that day and time "next week."
#' @keywords internal
.make_single_utc <- function(day, hour, timezone) {
  target_date <- .next_weekday(day, timezone)
  lubridate::hour(target_date) <- hour
  lubridate::minute(target_date) <- .tz_minutes(timezone, target_date)

  return(lubridate::with_tz(target_date, "UTC"))
}

#' Choose the next date of a given weekday name.
#'
#' @param target_weekday The name of the weekday.
#' @param tz The timezone in which the day should be returned.
#'
#' @return A POSIXct of the next occurrance of that day.
#' @keywords internal
.next_weekday <- function(target_weekday, tz) {
  # Convert the weekday name to the numeric representation.
  target_weekday <- as.integer(
    factor(
      target_weekday,
      # I could save this as a variable for the package, but I want to have it
      # here explicitly to remember that Sunday is the start of the week.
      levels = c(
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
      )
    )
  )

  # Always do this in the local timezone, so the selection of what "next Sunday"
  # (etc) means is always the same; the only difference will be the timezone in
  # which that date is encoded at the end.
  weekday_today <- lubridate::wday(
    lubridate::today(tzone = "America/Chicago"),
    week_start = 7
  )
  diff <- target_weekday - weekday_today

  # Make sure the date is in the future (and not today).
  if (diff <= 0) {
    diff <- diff + 7
  }

  # Add a week to give some breathing room.
  diff <- diff + 7

  return(
    lubridate::ymd(
      lubridate::today() + diff,
      tz = tz
    )
  )
}

#' Find the Minutes Offset of a Timezone
#'
#' @param timezone The timezone to check.
#' @param target_date The date to check.
#'
#' @return The integer minutes difference between the supplied timezone and the
#'   same time in UTC.
#' @keywords internal
.tz_minutes <- function(timezone, target_date) {
  midnight <- lubridate::with_tz(target_date, "UTC")
  lubridate::hour(midnight) <- 0L
  lubridate::minute(midnight) <- 0L
  midnight_tz <- lubridate::with_tz(midnight, tzone = timezone)
  return(lubridate::minute(midnight_tz))
}
