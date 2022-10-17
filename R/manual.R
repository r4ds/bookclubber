# Functions for manual processing of the data, setting up new clubs, and that
# sort of thing.

#' Pick a Convenient Time
#'
#' There's a lot of work to do on this still.
#'
#' @param book_name The abbreviation for an approved book, such as rpkgs or
#'   mshiny.
#' @param facilitator_id The Slack ID of the facilitator for this group.
#'   Available via the user's "full profile" in Slack.
#' @param output_tz A timezone in which to show the result, by default
#'   "America/Chicago" since that's what the R4DS calendar is in.
#'
#' @return The best times, as a tibble.
#' @export
choose_time <- function(book_name,
                        facilitator_id,
                        output_tz = "America/Chicago") {
  # Verify that book_name is in our list.
  book_name <- rlang::arg_match(book_name, values = approved_books)

  df <- googlesheets4::read_sheet(
    "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc",
    sheet = 1
  ) %>%
    dplyr::filter(.data$book_name == .env$book_name) %>%
    dplyr::select(-"book_name") %>%
    # Just keep the most recent submission for a given user for each day-time.
    dplyr::arrange(
      dplyr::desc(.data$submission_timestamp)
    ) %>%
    dplyr::distinct(
      .data$user_name,
      .data$user_id,
      .data$day,
      .data$hour,
      .keep_all = TRUE
    ) %>%
    dplyr::filter(.data$available) %>%
    # Convert times to UTC. We're going to assume they're answering for a random
    # day next week. Eventually we'll make that explicit.
    dplyr::rowwise() %>%
    dplyr::mutate(
      datetime_utc = .make_utc(
        day = .data$day,
        hour = .data$hour,
        timezone = .data$timezone
      )
    ) %>%
    dplyr::ungroup()

  facilitator_times <- df %>%
    dplyr::filter(.data$user_id == facilitator_id)

  if (!nrow(facilitator_times)) {
    rlang::rlang_abort(
      "That facilitator has not chosen any times for that book."
    )
  }

  facilitator_tz <- head(facilitator_times, 1)$timezone

  best_times <- df %>%
    dplyr::filter(
      .data$datetime_utc %in% facilitator_times$datetime_utc
    ) %>%
    dplyr::count(.data$datetime_utc, sort = TRUE)

  return(
    best_times %>%
      dplyr::mutate(
        datetime_output = lubridate::with_tz(
          .data$datetime_utc, output_tz
        ),
        # day = lubridate::wday(.data$datetime_output, label = TRUE),
        # hour = lubridate::hour(.data$datetime_output),
        datetime_facilitator = lubridate::with_tz(
          .data$datetime_utc, facilitator_tz
        ),
        day_facilitator = lubridate::wday(
          .data$datetime_facilitator, label = TRUE
        ),
        hour_facilitator = lubridate::hour(.data$datetime_facilitator),
        .before = .data$n
      ) %>%
      dplyr::select(
        -"datetime_utc"
      )
  )
}

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
    lubridate::today(),
    week_start = 7
  )
  diff <- target_weekday - weekday_today

  # Make sure the date is in the future (and not today).
  diff <- dplyr::if_else(
    diff <= 0,
    diff + 7,
    diff
  )
  return(
    lubridate::ymd(
      lubridate::today() + diff,
      tz = tz
    )
  )
}
