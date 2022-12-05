#' Stuff to run at startup
#'
#' I'll have to see what should universally run at startup (eg, definitions of
#' the possible slots) vs what should be checked during use (eg, we should load
#' the "used" slots from time to time, although we can probably do that in
#' global with a reactivePoll, so... yeah, this will get more stuff).
#'
#' @keywords internal
.app_global <- function() {
  .authorize_google_services()

  # I don't love using <<- to put things into the global environment, but right
  # now this seems to be the cleanest way to do this.

  # Load the books for everybody.
  .approved_books <<- shiny::reactivePoll(
    intervalMillis = 10L*60L*1000L, # No need to check often.
    # intervalMillis = 1000L, # For testing
    session = NULL,
    checkFunc = .check_club_sheet,
    valueFunc = .load_books
  )

  # Load the globally unavailable times.
  .unavailable_times <<- shiny::reactivePoll(
    intervalMillis = 10L*60L*1000L, # No need to check often.
    # intervalMillis = 1000L, # For testing
    session = NULL,
    checkFunc = .check_club_sheet,
    valueFunc = .load_unavailable_times
  )
}

#' Authorize Google for Sheets and Drive
#'
#' @return NULL (invisibly).
#' @keywords internal
.authorize_google_services <- function() {
  # Note that you must have the json named here in your inst folder. If you are
  # working on this app and believe you should be trusted with this access,
  # please contact the maintainer.
  googlesheets4::gs4_auth(
    path = .app_sys("bookclubs4ds-service-account.json")
  )

  googledrive::drive_auth(
    path = .app_sys("bookclubs4ds-service-account.json")
  )
}

#' Check Club Sheet Modified Time
#'
#' @return A string representing when the sheet was modified.
#' @keywords internal
.check_club_sheet <- function() {
  req <- googledrive::request_generate(
    endpoint = "drive.files.get",
    params = list(
      fileId = .gs4_sheet_id,
      fields = "modifiedTime"
    )
  )
  res <- googledrive::do_request(req)
  return(res$modifiedTime)
}

#' Load Unavailable Times
#'
#' @return A one-column tibble of unavailable times.
#' @keywords internal
.load_unavailable_times <- function() {
  club_times <- .read_gs4(
    sheet = "Clubs",
    range = "B:C",
    col_types = "ci"
  ) |>
    dplyr::transmute(
      date_utc = .make_utc(
        day = .data$`Day (UTC)`,
        hour = .data$`Hour (UTC)`,
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
