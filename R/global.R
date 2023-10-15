#' Stuff to run at startup
#'
#' @keywords internal
.app_global <- function() {
  .authorize_google_services()
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
#' @return A list with components of when the sheet was modified and the size of
#'   the sheet. It seems that this combo triggers updates more accurately than
#'   just the modifiedTime alone.
#' @keywords internal
.check_club_sheet <- function() {
  req <- googledrive::request_generate(
    endpoint = "drive.files.get",
    params = list(
      fileId = .gs4_sheet_id,
      fields = "modifiedTime, size"
      # fields = "modifiedTime"
    )
  )
  res <- googledrive::do_request(req)
  return(res)
}

#' Read a Sheet from the GS4 Workbook
#'
#' @param ... Arguments passed on to [googlesheets4::read_sheet()].
#'
#' @return A google sheet.
#' @keywords internal
.read_gs4 <- function(...) {
  googlesheets4::read_sheet(.gs4_sheet_id, ...)
}
