#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
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

  # Display their name so they can log out if it's the wrong person. This should
  # eventually move into shinyslack as an exported module.
  slack_user_info <- shinyslack::user_info()

  output$username <- shiny::renderText(
    paste(
      shiny::strong("Logged in as"),
      shiny::br(),
      slack_user_info()[["user_name"]]
    )
  )

  # display the week calendar
  output$time_table <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(week_calendar) #, width = 550, height = 300)
  })

  shiny::observeEvent(
    # observe if any changes to the cells of the rhandsontable
    input$time_table$changes$changes,
    {
      time_selections_long <- shiny::reactive({
        rhandsontable::hot_to_r(input$time_table) %>%
          tibble::rownames_to_column(var = "time") %>%
          tidyr::pivot_longer(
            cols = .data$Monday:.data$Sunday,
            names_to = "day",
            values_to = "availability"
          ) %>%
          identity()
      })

      output$selected <- shiny::renderTable({
        time_selections_long() %>%
          dplyr::filter(.data$availability == TRUE) %>%
          dplyr::group_by(.data$day) %>%
          dplyr::mutate(
            availability = stringr::str_flatten(time, collapse = ", ")
          ) %>%
          dplyr::select(-time) %>% # -availability,
          dplyr::distinct() %>%
          identity()
      })
    })

  # Save the user details
  user_info <- shiny::reactive({
    data.frame(
      user_name = slack_user_info()[["user_name"]],
      user_id = slack_user_info()[["user_id"]],
      book_name = input$bookname,
      timezone = input$timezone,
      submission_timestamp = as.character(Sys.time())
    )
  })

  user_availability_df <- shiny::eventReactive(
    input$submit,
    {
      cbind(
        user_info(),
        # Combine the user info (recycled for all availability rows) with
        # availability details
        rhandsontable::hot_to_r(input$time_table)
      ) %>%
        dplyr::mutate(hour = 0:23) %>%
        tidyr::pivot_longer(
          cols = .data$Monday:.data$Sunday,
          names_to = "day",
          values_to = "available"
        ) %>%
        dplyr::select(
          .data$user_name,
          .data$user_id,
          .data$book_name,
          .data$timezone,
          .data$submission_timestamp,
          .data$day,
          .data$hour,
          .data$available
        )

      })

  output$text2 <- shiny::renderTable({
    user_availability_df()
  })

  shiny::observeEvent(
    input$submit,
    {
      # Show a modal right away to prevent further submits.
      shiny::showModal(
        shiny::modalDialog(
          title = "Thank you, your availability has been submitted.",
          easyClose = TRUE,
          footer = shiny::tagList(shiny::modalButton("Ok"))
        )
      )
      # On click of Submit button, save the response on the googlesheets file
      googlesheets4::sheet_append(
        "https://docs.google.com/spreadsheets/d/1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc/edit#gid=0",
        user_availability_df(),
        sheet = 1
      )
      # Technically if it doesn't save we don't tell them. Issue #16
    }
  )

  shiny::observe({

    #Get URL query
    query <- shiny::parseQueryString(session$clientData$url_search)

    #Ignore if the URL query is null
    if (!is.null(query[["bookname"]]) && query[["bookname"]] %in% approved_books) {

      #Update the select input
      shiny::updateSelectInput(
        session,
        "bookname",
        selected  = query[["bookname"]],
        choices = approved_books
      )

    }

  })
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
  # Google login. Note that you must have the json named here in your inst
  # folder. If you are working on this app and believe you should be trusted
  # with this access, please contact the maintainer.
  googlesheets4::gs4_auth(
    path = system.file(
      "bookclubs4ds-service-account.json", package = "bookclubber"
    )
  )
}
