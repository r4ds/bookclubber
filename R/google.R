# Copy/pasting from bookclubdata until everything is transferred so everybody
# uses the same service account format.

.service_account_json <- function() {
  intToUtf8(
    base64enc::base64decode(
      Sys.getenv("R4DS_CLUB_GOOGLE_SERVICE_ACCOUNT")
    )
  )
}

.googledrive_authorize <- function() {
  googledrive::drive_auth(
    path = .service_account_json()
  )
}

.googlesheets_authorize <- function() {
  googlesheets4::gs4_auth(
    path = .service_account_json()
  )
}
