#' UI to Choose Times
#'
#' @inheritParams .shared-parameters
#'
#' @return A [rhandsontable::rHandsontableOutput()] UI.
#' @keywords internal
.calendar_ui <- function(id = "calendar") {
  return(
    rhandsontable::rHandsontableOutput(
      shiny::NS(id, "availability")
    )
  )
}

#' Server-side Calendar Selector
#'
#' @inheritParams .shared-parameters
#'
#' @return A [rhandsontable::renderRHandsontable()].
#' @keywords internal
.calendar_selector <- function(user_id, user_timezone, selected_book, signups) {
  # Note: Put in NA to block things out, but we need to turn those into FALSE
  # when we save.
  rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      .preset_table(user_id, user_timezone, selected_book, signups),
      contextMenu = FALSE
    ) |>
      .format_table(user_timezone, selected_book, signups)
  })
}

#' Add a JavaScript formatter to the table
#'
#' @inheritParams .shared-parameters
#' @param hot A [rhandsontable::rhandsontable()] object.
#'
#' @return A [rhandsontable::rhandsontable()] with JavaScript for formatting.
#' @keywords internal
.format_table <- function(hot, user_timezone, selected_book, signups) {
  # It doesn't look like there's anything built-in to color the table based on
  # values in *another* table.

  density_matrix <- .signups_to_matrix(signups, user_timezone, selected_book)

  hot |>
    rhandsontable::hot_cols(renderer = .generate_renderer_js(density_matrix))
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
.preset_table <- function(user_id, user_timezone, selected_book, signups) {
  cli::cli_inform(
    "Finding available times for {user_timezone}."
  )
  user_book_signups <- signups |>
    dplyr::filter(
      .data$user_id == .env$user_id,
      .data$book_name == .env$selected_book
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
        TRUE ~ .data$available
      )
    ) |>
    dplyr::select(-"unavailable") |>
    tidyr::pivot_wider(
      names_from = "day",
      values_from = "available"
    ) |>
    dplyr::mutate(hour = .time_slots) |>
    tibble::column_to_rownames("hour")

  return(preset_table)
}

#' Convert unavailable times to this user's timezone
#'
#' Note: This currently depends on the global object `.unavailable_times()`.
#'
#' @inheritParams .shared-parameters
#'
#' @return A tibble with columns day, hour, and unavailable.
#' @keywords internal
.get_unavailable_times_tz <- function(user_timezone) {
  return(
    shiny::isolate(.unavailable_times()) |>
      dplyr::mutate(
        unavailable_time = lubridate::with_tz(
          .data$unavailable_time, user_timezone
        )
      ) |>
      dplyr::transmute(
        day = lubridate::wday(
          .data$unavailable_time,
          week_start = 1,
          label = TRUE,
          abbr = FALSE
        ),
        hour = lubridate::hour(.data$unavailable_time),
        unavailable = TRUE
      )
  )
}

#' Load signup information from the google sheet
#'
#' @param unavailable_times A datetime vector of unavailble times, in UTC.
#'
#' @return A data.frame with information about user signups, in UTC.
#' @keywords internal
.load_signups <- function(unavailable_times) {
  return(
    .read_gs4(
      sheet = "Signups",
      range = "A:H",
      col_types = "ccccccil"
    ) |>
      dplyr::mutate(
        # Actual TZ that Google "thinks" in here isn't important.
        submission_timestamp = lubridate::ymd_hms(
          .data$submission_timestamp
        )
      ) |>
      # Only keep the most recent entry for each person-book.
      dplyr::arrange(
        dplyr::desc(.data$submission_timestamp)
      ) |>
      dplyr::select(-"submission_timestamp") |>
      dplyr::distinct(
        .data$user_id,
        .data$book_name,
        .data$day,
        .data$hour,
        .keep_all = TRUE
      ) |>
      dplyr::filter(.data$available) |>
      # Convert all times to UTC. We'll put them into this user's TZ as needed
      # (since TZ can change more often than this should).
      dplyr::rowwise() |>
      dplyr::mutate(
        datetime_utc = .make_utc(
          day = .data$day,
          hour = .data$hour,
          timezone = .data$timezone
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"day", -"hour", -"available") |>
      # Get rid of any that are no longer valid.
      dplyr::filter(
        !(.data$datetime_utc %in% unavailable_times)
      )
  )
}

#' Summarize, pivot, and matricize signups
#'
#' @inheritParams .shared-parameters
#' @param signups The result of [.load_signups()].
#'
#' @return A matrix of signups, with an integer count in each cell.
#' @keywords internal
.signups_to_matrix <- function(signups, user_timezone, selected_book) {
  signup_counts <- signups |>
    dplyr::filter(.data$book_name == .env$selected_book) |>
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
