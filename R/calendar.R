.ui_calendar_row <- function() {
  fluidRow(
    column(
      width = 12,
      h4("Select your availability"),
      p(
        "(missing check boxes are unavailable; other DSLC clubs are using the
            Zoom account(s) at those times; colored cells indicate existing sign ups)"),
      .calendar_ui()
    )
  )
}

#' UI to Choose Times
#'
#' @inheritParams .shared-parameters
#'
#' @return A [rhandsontable::rHandsontableOutput()] UI.
#' @keywords internal
.calendar_ui <- function(id = "calendar") {
  return(
    rhandsontable::rHandsontableOutput(
      NS(id, "availability")
    )
  )
}

#' Server-side Calendar Selector
#'
#' @inheritParams .shared-parameters
#'
#' @return A [rhandsontable::renderRHandsontable()].
#' @keywords internal
.calendar_selector <- function(user_id, user_timezone, signups) {
  # Put in NA to block things out, but we need to turn those into FALSE
  # when we save.
  rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      .preset_table(user_id, user_timezone, signups),
      contextMenu = FALSE,
      rowHeaderWidth = 80
    ) |>
      .format_table(user_timezone, signups)
  })
}

#' Add a JavaScript formatter to the table
#'
#' @inheritParams .shared-parameters
#' @param hot A [rhandsontable::rhandsontable()] object.
#'
#' @return A [rhandsontable::rhandsontable()] with JavaScript for formatting.
#' @keywords internal
.format_table <- function(hot, user_timezone, signups) {
  # It doesn't look like there's anything built-in to color the table based on
  # values in *another* table.

  density_matrix <- .signups_to_matrix(signups, user_timezone)

  hot |>
    rhandsontable::hot_cols(
      renderer = .generate_renderer_js(density_matrix),
      type = "checkbox",
      allowInvalid = TRUE
    )
}

#' Generate JavaScript for formatting the table
#'
#' @param density_matrix The signup information (in this user's timezone) as a
#'   matrix.
#'
#' @return JavaScript code as a character scalar.
#' @keywords internal
.generate_renderer_js <- function(density_matrix) {
  # This is inspired by rhandsontable::hot_heatmap.
  js <- readLines(
    system.file("hot_heatmap_secondary.js.template", package = "bookclubber")
  ) |>
    stringr::str_replace(
      "%densitymap",
      jsonlite::toJSON(density_matrix)
    ) |>
    paste(collapse = "\n")
  return(js)
}

#' Set up the table for this user
#'
#' @inheritParams .shared-parameters
#'
#' @return A data.frame for use in [rhandsontable::rhandsontable()].
#' @keywords internal
.preset_table <- function(user_id, user_timezone, signups) {
  user_book_signups <- signups |>
    dplyr::filter(
      .data$user_id == .env$user_id
    ) |>
    dplyr::mutate(
      datetime_user = lubridate::with_tz(
        .data$datetime_utc, .env$user_timezone
      ),
      day = lubridate::wday(
        .data$datetime_user,
        week_start = 1,
        label = TRUE,
        abbr = FALSE
      ),
      hour = lubridate::hour(.data$datetime_user),
      user_available = TRUE
    ) |>
    dplyr::select("day", "hour", "user_available")

  unavailable_times_tz <- .get_unavailable_times_tz(user_timezone)

  preset_table <- .week_calendar_long |>
    dplyr::mutate(available = FALSE) |>
    dplyr::left_join(user_book_signups, by = c("day", "hour")) |>
    dplyr::mutate(
      available = dplyr::coalesce(
        .data$user_available, .data$available
      )
    ) |>
    dplyr::select(-"user_available") |>
    dplyr::left_join(unavailable_times_tz, by = c("day", "hour")) |>
    dplyr::mutate(
      available = dplyr::case_when(
        .data$unavailable ~ NA,
        .default = .data$available
      )
    ) |>
    dplyr::select(-"unavailable") |>
    tidyr::pivot_wider(
      names_from = "day",
      values_from = "available"
    ) |>
    dplyr::mutate(hour = .hour_to_name(.data$hour, user_timezone)) |>
    tibble::column_to_rownames("hour")

  return(preset_table)
}

#' Create rownames from hours
#'
#' @inheritParams .shared-parameters
#' @param hour An integer vector of hours. This will almost certainly just be
#'   0:23 repeated 7 times.
#'
#' @return A character vector the same length as hour, with formatted times.
#' @keywords internal
.hour_to_name <- function(hour, user_timezone) {
  # Convert to 24-hour clock.
  hour <- dplyr::case_when(
    hour == 0 ~ 12L,
    hour > 12 ~ hour - 12L,
    .default = hour
  )
  return(
    paste0(
      hour,
      ":",
      stringr::str_pad(
        bookclubdata::tz_minutes(user_timezone),
        width = 2L,
        pad = "0"
      ),
      c(
        rep("AM", 12),
        rep("PM", 12)
      )
    )
  )
}

#' Convert unavailable times to this user's timezone
#'
#' This currently depends on the global object `.unavailable_times()`.
#'
#' @inheritParams .shared-parameters
#'
#' @return A tibble with columns day, hour, and unavailable.
#' @keywords internal
.get_unavailable_times_tz <- function(user_timezone) {
  return(
    .unavailable_times() |>
      dplyr::mutate(
        unavailable_time = lubridate::with_tz(
          .data$unavailable_time, user_timezone
        )
      ) |>
      dplyr::mutate(
        day = lubridate::wday(
          .data$unavailable_time,
          week_start = 1,
          label = TRUE,
          abbr = FALSE
        ),
        hour = lubridate::hour(.data$unavailable_time),
        .keep = "none"
      ) |>
      dplyr::distinct(.data$day, .data$hour) |>
      dplyr::mutate(unavailable = TRUE)
  )
}

#' Summarize, pivot, and matricize signups
#'
#' @inheritParams .shared-parameters
#' @param signups The result of [.load_book_signups()].
#'
#' @return A matrix of signups, with an integer count in each cell.
#' @keywords internal
.signups_to_matrix <- function(signups, user_timezone) {
  signup_counts <- signups |>
    dplyr::mutate(
      datetime_user = lubridate::with_tz(
        .data$datetime_utc, user_timezone
      ),
      day = lubridate::wday(
        .data$datetime_user, week_start = 1, label = TRUE, abbr = FALSE
      ),
      hour = lubridate::hour(.data$datetime_user)
    ) |>
    dplyr::count(.data$day, .data$hour) |>
    dplyr::arrange(.data$day, .data$hour)

  return(
    .week_calendar_long |>
      dplyr::left_join(signup_counts, by = c("day", "hour")) |>
      tidyr::replace_na(list(n = 0L)) |>
      tidyr::pivot_wider(names_from = "day", values_from = "n") |>
      dplyr::select(-"hour") |>
      as.matrix() |>
      unname()
  )
}

.time_table_process <- function(time_table, user_timezone) {
  .time_table_pivot(time_table) |>
    .time_table_availability(user_timezone) |>
    dplyr::filter(.data$available) |>
    dplyr::select(-"available")
}

.time_table_pivot <- function(time_table) {
  tidyr::pivot_longer(
    dplyr::mutate(rhandsontable::hot_to_r(time_table), hour = 0:23),
    cols = "Monday":"Sunday",
    names_to = "day",
    values_to = "available"
  )
}

.time_table_availability <- function(time_table, user_timezone) {
  dplyr::left_join(
    time_table,
    .get_unavailable_times_tz(user_timezone),
    by = c("day", "hour")
  ) |>
    .time_table_availability_resolve()
}

.time_table_availability_resolve <- function(time_table) {
  time_table$available <- tidyr::replace_na(time_table$available, FALSE)
  time_table$unavailable <- tidyr::replace_na(time_table$unavailable, FALSE)
  time_table$available <- time_table$available & !time_table$unavailable
  time_table$unavailable <- NULL
  return(time_table)
}
