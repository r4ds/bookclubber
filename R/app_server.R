#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @keywords internal
.app_server <- function(input, output, session) {
  # I need the user info outside of the username thing, so for now I collect it
  # separately. Ideally it should be more strongly associated with the user
  # code.
  slack_user_info <- shinyslack::user_info()

  # Update and otherwise deal with the simple inputs across the top.
  .user_server(slack_user_info = slack_user_info)
  .book_server() # Should become fully module-ized later.
  .timezone_server()

  # I need to sort out the reactivity to make this its own module. For now this
  # works and other attempts didn't.
  shiny::observe({
    shiny::req(input[[shiny::NS("timezone", "selected_zone")]])
    output[[shiny::NS("calendar", "availability")]] <- .calendar_selector(
      input[[shiny::NS("timezone", "selected_zone")]]
    )
  })

  # This all works right now. Ideally I'd like to unselect unavailable times in
  # the UI if a user clicks-and-drags and clicks space to select over
  # unavailable times. But at least it works!

  # Save their selections on Submit.
  shiny::observeEvent(
    input$submit,
    .save_availability(
      slack_user_info()[["user_name"]],
      slack_user_info()[["user_id"]],
      input$book_name,
      input[[shiny::NS("timezone", "selected_zone")]],
      input[[shiny::NS("calendar", "availability")]]
    )
  )
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
