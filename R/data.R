.unavailable_times <- function() {
  club_times <- bookclubdata::active_clubs_times(TRUE) |>
    dplyr::transmute(
      date_utc = .make_utc(
        day = .data$day_utc,
        hour = .data$hour_utc,
        timezone = "UTC"
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
