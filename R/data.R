.unavailable_times <- function() {
  club_times <- bookclubdata::active_clubs_times() |>
    dplyr::mutate(
      next_hour = .data$datetime_utc + lubridate::hours(1),
      prev_hour = .data$datetime_utc - lubridate::hours(1),
      .keep = "used"
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
