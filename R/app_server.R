#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @keywords internal
.app_server <- function(input, output, session) {
  # I need the user info outside of the username display, so for now I collect
  # it separately. Ideally it should be more strongly associated with the user
  # code.
  slack_user_info <- shinyslack::user_info(c("display_name", "user_id"))

  # Grab a timestamp for the spreadsheet. We'll use this for grabbing data from
  # the cache. We want a session version of this for this user so that we only
  # grab a new version when they submit. Note: This number lags by up to several
  # minutes, but it's good enough for this application.
  club_sheet_modified <- shiny::reactive({
    # Don't fetch this until they're logged in.
    shiny::req(slack_user_info()[["user_id"]])
    .check_club_sheet()
  }) |>
    shiny::bindEvent(input$submit, ignoreNULL = FALSE)

  approved_books <- bookclubdata::approved_books(refresh = TRUE)

  # Ditto for the signups.
  signups <- shiny::reactive({
    # Don't fetch this when we don't have info about the sheet.
    shiny::req(club_sheet_modified())
    .load_signups(.unavailable_times()$unavailable_time)
  }) |>
    shiny::bindCache(club_sheet_modified())

  # We still grab the unavailable times globally, because they change very
  # rarely.

  # Update and otherwise deal with the simple inputs across the top.
  .user_server(slack_user_info = slack_user_info)
  # Should become fully module-ized later.
  .book_observer(approved_books = approved_books)
  .timezone_server()

  # I need to sort out the reactivity to make this its own module. For now this
  # works and other attempts didn't.
  shiny::observe({
    shiny::req(
      input[[shiny::NS("timezone", "selected_zone")]],
      approved_books,
      signups(),
      input$book_name,
      slack_user_info()[["user_id"]]
    )
    output[[shiny::NS("calendar", "availability")]] <- .calendar_selector(
      slack_user_info()[["user_id"]],
      input[[shiny::NS("timezone", "selected_zone")]],
      input$book_name,
      signups()
    )
  })

  # Save their selections on Submit.
  shiny::observeEvent(
    input$submit,
    .save_availability(
      slack_user_info()[["display_name"]],
      slack_user_info()[["user_id"]],
      input$book_name,
      input[[shiny::NS("timezone", "selected_zone")]],
      input[[shiny::NS("calendar", "availability")]]
    ),
    priority = 1000
  )
}
