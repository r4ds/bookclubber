#' Stuff to run at startup
#'
#' I'll have to see what should universally run at startup (eg, definitions of
#' the possible slots) vs what should be checked during use (eg, we should load
#' the "used" slots from time to time, although we can probably do that in
#' global with a reactivePoll, so... yeah, this will get more stuff).
#'
#' @keywords internal
.app_global <- function() {
  .authorize_gs4_service()
}

#' Authorize Google for Sheets
#'
#' @return NULL (invisibly).
#' @keywords internal
.authorize_gs4_service <- function() {
  # Note that you must have the json named here in your inst folder. If you are
  # working on this app and believe you should be trusted with this access,
  # please contact the maintainer.
  googlesheets4::gs4_auth(
    path = system.file(
      "bookclubs4ds-service-account.json",
      package = "bookclubber"
    )
  )
}

