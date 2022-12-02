.calendar_selector <- function() {
  # This is a separate function to make it easier to eventually re-fill it and
  # block out existing clubs. Note: Put in NA to block things out, but we need
  # to turn those into FALSE when we save.
  rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(.week_calendar)
  })
}
