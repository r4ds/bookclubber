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
    checkFunc = .check_books,
    valueFunc = .load_books
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
    path = system.file(
      "bookclubs4ds-service-account.json",
      package = "bookclubber"
    )
  )

  googledrive::drive_auth(
    path = system.file(
      "bookclubs4ds-service-account.json",
      package = "bookclubber"
    )
  )
}

