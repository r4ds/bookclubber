.calendar_ui <- function(id = "calendar") {
  return(
    rhandsontable::rHandsontableOutput(
      shiny::NS(id, "availability")
    )
  )
}

.calendar_selector <- function(user_timezone) {
  # Note: Put in NA to block things out, but we need to turn those into FALSE
  # when we save.
  rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      .available_times(user_timezone),
      contextMenu = FALSE
    )
  })
}

.available_times <- function(user_timezone) {
  cli::cli_inform(
    "Finding available times for {user_timezone}."
  )
  unavailable_times_tz <- .get_unavailable_times(user_timezone)
  return(
    dplyr::tibble(
      day = rep(.days, each = 24),
      hour = rep(0:23, 7),
      available = FALSE
    ) |>
      dplyr::left_join(unavailable_times_tz, by = c("day", "hour")) |>
      dplyr::mutate(
        available = dplyr::coalesce(
          .data$unavailable, .data$available
        ) |>
          dplyr::na_if(TRUE)
      ) |>
      dplyr::select(-"unavailable") |>
      tidyr::pivot_wider(
        names_from = "day",
        values_from = "available"
      ) |>
      dplyr::mutate(hour = .time_slots) |>
      tibble::column_to_rownames("hour")
  )
}

.get_unavailable_times <- function(user_timezone) {
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
