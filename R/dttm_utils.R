.pretty_now <- function() {
  strftime(
    lubridate::now(tzone = "UTC"),
    tz = "UTC",
    usetz = TRUE,
    format = "%Y-%m-%d %H:%M:%S"
  )
}
