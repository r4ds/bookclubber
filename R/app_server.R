#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @keywords internal
.app_server <- function(input, output, session) {
  # Update the timezone dropdown to use the detected zone by default. This is
  # inspired by code from https://github.com/rpodcast/shinycal.
  shiny::observe({
    tz <- input$clientZone
    # If it's blank or malformed, don't select anything by default.
    if (is.null(tz) || !tz %in% OlsonNames()) tz <- character(0)
    shiny::updateSelectInput(
      inputId = "timezone",
      selected = tz
    )
  })

  # display the week calendar
  output$time_table <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(week_calendar) #, width = 550, height = 300)
  })

  observeEvent(
    # observe if any changes to the cells of the rhandontable
    input$time_table$changes$changes,
    {
      time_selections <- reactive({
        rhandsontable::hot_to_r(input$time_table)
      })

      time_selections_long <- reactive({
        rhandsontable::hot_to_r(input$time_table) %>%
          tibble::rownames_to_column(var = "time") %>%
          tidyr::pivot_longer(
            cols = Monday:Sunday,
            names_to = "day",
            values_to = "availability"
          ) %>%
          identity()
      })

      output$selected <- renderTable({
        time_selections_long() %>%
          dplyr::filter(availability == TRUE) %>%
          dplyr::group_by(day) %>%
          dplyr::mutate(
            availability = stringr::str_flatten(time, collapse = ", ")
          ) %>%
          dplyr::select(-time) %>% # -availability,
          dplyr::distinct() %>%
          identity()
      })
    })

  # output$text <- renderText({
  #     str(time_selections())
  # })

  # Save the user details
  user_info <-  reactive({
    data.frame(
      book_name            = input$bookname,
      name                 = input$username,
      tz                   = input$timezone,
      submission_timestamp = as.character(Sys.time())
    )
  })

  user_availability_df <- eventReactive(
    input$submit,
    {
      cbind(
        user_info(),
        # Combine the user info (recycled for all availability rows) with
        # availability details
        rhandsontable::hot_to_r(input$time_table)
      )
    }
  )

  # output$text <- renderTable({
  #     user_info()[,1:2]
  #     # head(user_availability_df())
  # })

  output$text2 <- renderTable({
    user_availability_df()
  })

  observeEvent(
    input$submit,
    {
      # On click of Submit button, save the response on the googlesheets file
      googlesheets4::sheet_append(
        "https://docs.google.com/spreadsheets/d/1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc/edit#gid=0",
        user_availability_df(),
        sheet = 1
      )
    }
  )
}

#' Stuff to run at startup
#'
#' I'll have to see what should universally run at startup (eg, definitions of
#' the possible slots) vs what should be checked during use (eg, we should load
#' the "used" slots from time to time, although we can probably do that in
#' global with a reactivePoll, so... yeah, this will get more stuff).
#'
#' After reading up a bit on the golem github, it looks like this isn't QUITE
#' what I want. We'll explore this further as we go, but likely we just want
#' most of this in server, with maybe a little bit in the golem_opts in run_app.
#'
#' @keywords internal
.app_global <- function() {
  # Ick. I currently have to use <<- to get this to work, since this is only
  # kinda sorta global, evidently. This needs to be sorted out.
  approved_books <<- c("r4ds","advanced-r","feat","ggplot2","r-packages")

  days <<- c(
    "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"
  )

  time_slots <<- data.frame(
    time_slot = c(
      paste(
        c(12, 1:11),
        "AM"
      ),
      paste(
        c(12, 1:11),
        "PM"
      )
    )
  )

  sl <<- data.frame(sno = seq_len(nrow(time_slots)))

  running_book_clubs <<- matrix(F, nrow = 24, ncol = 7)
  # creating dummy data to test the concept of removing unavailable times. To be
  # replaced with actual data from Jon.
  running_book_clubs[1,] <<- TRUE
  running_book_clubs[,1] <<- TRUE

  week_calendar <<- (
    running_book_clubs -
      matrix(
        F,
        nrow = 24,
        ncol = 7,
        dimnames = list(time_slots$time_slot, days)
      )
  )  %>%
    data.frame() %>%
    dplyr::mutate(
      dplyr::across(c(Monday:Sunday), dplyr::na_if, TRUE)
    ) %>%
    dplyr::mutate_at(dplyr::vars(Monday:Sunday), as.logical)

  # Google login. Note that you must have the json named here in your inst
  # folder. If you are working on this app and believe you should be trusted
  # with this access, please contact the maintainer.
  googlesheets4::gs4_auth(
    path = system.file(
      "bookclubs4ds-service-account.json", package = "bookclubber"
    )
  )
}
