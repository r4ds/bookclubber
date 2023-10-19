.unavailable_times <- function() {
  club_times <- bookclubdata::active_clubs_times(TRUE) |>
    dplyr::transmute(
      date_utc = bookclubdata::make_datetimes_utc(
        days = .data$day_utc,
        hours = .data$hour_utc,
        timezones = "UTC"
      ),
      next_hour = .data$date_utc + lubridate::hours(1),
      prev_hour = .data$date_utc - lubridate::hours(1)
    ) |>
    tidyr::pivot_longer(
      tidyr::everything(),
      values_to = "unavailable_time"
    ) |>
    dplyr::distinct(.data$unavailable_time) |>
    dplyr::arrange(.data$unavailable_time)

  return(club_times)
}

.load_book_signups <- function(book_name) {
  dplyr::filter(
    bookclubdata::signups_read(book_name, refresh = TRUE),
    !(datetime_utc %in% .unavailable_times()$unavailable_time)
  )
}
