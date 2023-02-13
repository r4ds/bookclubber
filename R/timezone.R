#' UI to Select a Timezone
#'
#' @inheritParams .shared-parameters
#'
#' @return A [shiny::tagList()] that adds the appropriate info to the app for
#'   timezone settings.
#' @keywords internal
.timezone_ui <- function(id = "timezone") {
  return(
    shiny::tagList(
      # Add HTML to the page to store the timezone.
      shiny::tags$input(
        type = "text",
        id = shiny::NS(id, "client_zone"),
        name = "Client zone",
        style = "display: none;"
      ),
      shiny::selectInput(
        inputId = shiny::NS(id, "selected_zone"),
        label = "Select Your Time Zone",
        choices = c(
          "DETECTING TIMEZONE" = "",
          OlsonNames()
        )
      )
    )
  )
}

#' A module for setting timezones.
#'
#' @inheritParams .shared-parameters
#'
#' @return A [shiny::moduleServer()] to update the timezone input using
#'   javascript.
#' @keywords internal.
.timezone_server <- function(id = "timezone") {
  shiny::moduleServer(id, function(input, output, session) {
    # Update the timezone dropdown to use the detected zone by default. This is
    # inspired by code from https://github.com/rpodcast/shinycal.
    shiny::observe({
      tz <- input$client_zone
      cli::cli_inform("Updating timezone: {tz}")
      # If it's blank or malformed, don't select anything by default.
      if (is.null(tz) || !tz %in% OlsonNames()) tz <- NULL
      shiny::updateSelectInput(
        inputId = "selected_zone",
        selected = tz
      )
    })
  })
}
