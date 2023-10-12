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
#' @param required_choosers The minimum number of sign-ups necessary for the
#'   club to launch. Think really hard before changing this.
#'
#' @return The best times, as a tibble.
#' @export
choose_time <- function(book_name,
                        facilitator_id,
                        required_choosers = 5) {
  # Verify that book_name is in our list.
  # book_name <- rlang::arg_match(book_name, values = approved_books)

  df <- .load_signups(.load_unavailable_times()$unavailable_time) |>
    dplyr::filter(.data$book_name == .env$book_name) |>
    dplyr::select(-"book_name")

  facilitator_times <- df |>
    dplyr::filter(.data$user_id == facilitator_id)

  if (!nrow(facilitator_times)) {
    rlang::abort(
      "That facilitator has not chosen any valid times for that book."
    )
  }

  facilitator_tz <- facilitator_times$timezone[[1]]

  best_times <- df |>
    dplyr::filter(
      .data$datetime_utc %in% facilitator_times$datetime_utc
    ) |>
    dplyr::mutate(user_name = glue::glue("@{.data$user_name}")) |>
    dplyr::group_by(.data$datetime_utc) |>
    dplyr::summarize(
      n = dplyr::n(),
      users = glue::glue_collapse(sort(unique(.data$user_name)), sep = ", ")
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))

  facilitator_minutes <- .tz_minutes(facilitator_tz)

  if (facilitator_minutes > 0) {
    cli::cli_alert_warning(
      "Facilitator timezone has a {facilitator_minutes} minute offset."
    )
  }

  if (max(best_times$n) < required_choosers) {
    withr::local_options(cli.condition_width = Inf)
    user_tags <- paste0("@", sort(unique(df$user_name)))
    starter_msg <- glue::glue(
      ": You have all indicated interest in joining this cohort to read {book_name}, but we",
      "don't yet have enough people with overlappping schedules to launch.",
      .sep = " "
    )
    general_message <- "@here: If you're in this channel, you must have some interest in joining a cohort."
    book_esc <- utils::URLencode(book_name)
    book_url <- glue::glue("https://r4ds.io/bookclubber?bookname={book_esc}")
    after_app <- glue::glue(
      "especially any green times (the shading indicates how many people have chosen that time).",
      "I'll check again next Monday to see if we are ready to launch!",
      .sep = " "
    )
    for_me <- glue::glue("choose_time(\"{book_name}\", \"{facilitator_id}\")")
    cli::cli_inform(
      c(
        "!" = "No times with {required_choosers}+ users.",
        "",
        "{user_tags}{starter_msg}",
        "",
        "{general_message} Please check the app ({book_url}) -- {after_app}",
        "",
        "For me: This is the code to check this club again:",
        "{.code {for_me}}"
      )
    )
  } else {
    clean_times <- best_times |>
      dplyr::filter(.data$n >= required_choosers) |>
      dplyr::mutate(
        datetime_r4ds = lubridate::with_tz(
          .data$datetime_utc, "America/Chicago"
        ),
        datetime_facilitator = lubridate::with_tz(
          .data$datetime_utc, facilitator_tz
        ),
        day_you = lubridate::wday(
          .data$datetime_facilitator, label = TRUE, abbr = FALSE
        ),
        hour_you = lubridate::hour(.data$datetime_facilitator),
        day_r4ds = lubridate::wday(
          .data$datetime_r4ds, label = TRUE, abbr = FALSE
        ),
        hour_r4ds = lubridate::hour(.data$datetime_r4ds)
      ) |>
      dplyr::select(
        "n",
        "day_you",
        "hour_you",
        "day_r4ds",
        "hour_r4ds",
        "users"
      )

    minutes_you <- stringr::str_pad(facilitator_minutes, 2, pad = "0")

    cli::cli_bullets(
      glue::glue_data(
        clean_times,
        "{n} people: {day_you}s {hour_you}:{minutes_you} ({users})",
        "({day_r4ds}s {hour_r4ds}:00 R4DS time)",
        .sep = "\n"
      )
    )

    return(invisible(clean_times))
  }
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
  lubridate::minute(target_date) <- .tz_minutes(timezone)

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
#'
#' @return The integer minutes difference between the supplied timezone and the
#'   same time in UTC.
#' @keywords internal
.tz_minutes <- function(timezone) {
  # Note: There's technically one timezone, "Australia/LHI" that has a 30-minute
  # DST switch. They break things and hopefully they know what they did.
  midnight <- lubridate::ymd_hms("2023-01-01 00:00:00", tz = "UTC")
  midnight_tz <- lubridate::with_tz(midnight, tzone = timezone)
  return(lubridate::minute(midnight_tz))
}
