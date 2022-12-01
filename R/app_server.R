#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @keywords internal
.app_server <- function(input, output, session) {
  # Load the books once. No need to make this reactive.
  .approved_books <- .load_books()

  # Set the book drop-down.
  shiny::observe(
    {
      book_selected <- "PLEASE SELECT A BOOK"
      book_choices <- c(
        book_selected,
        .approved_books$book_name
      )

      query <- shiny::parseQueryString(session$clientData$url_search)

      if (
        !is.null(query[["bookname"]]) &&
        query[["bookname"]] %in% book_choices
      ) {
        book_selected <- query[["bookname"]]
      }

      shiny::updateSelectInput(
        session,
        "bookname",
        choices = book_choices,
        selected = book_selected
      )
    }
  )

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
    rhandsontable::rhandsontable(week_calendar)
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
            availability = stringr::str_flatten(.data$time, collapse = ", ")
          ) %>%
          dplyr::select(-"time") %>% # -availability,
          dplyr::distinct() %>%
          identity()
      })
    }
  )

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
          "user_name",
          "user_id",
          "book_name",
          "timezone",
          "submission_timestamp",
          "day",
          "hour",
          "available"
        )
    }
  )

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
        "1G5KjY77ONuaHj530ttzrhCS9WN4_muYxfLgP3xK24Cc",
        user_availability_df(),
        sheet = 1
      )
      # Technically if it doesn't save we don't tell them. Issue #16
    }
  )
}

#' Load Books
#'
#' @return A tibble with the valid books.
#' @keywords internal
.load_books <- function() {
  dplyr::arrange(
    .read_gs4(
      sheet = "Books",
      range = "A:A",
      col_types = "c"
    ),
    "book_name"
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
