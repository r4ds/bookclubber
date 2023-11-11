#' UI to Select a Timezone
#'
#' @inheritParams .shared-parameters
#'
#' @return A [shiny::tagList()] that adds the appropriate info to the app for
#'   timezone settings.
#' @keywords internal
.timezone_ui <- function(id = "timezone") {
  return(
    tagList(
      # Add HTML to the page to store the timezone.
      tags$input(
        type = "text",
        id = NS(id, "client_zone"),
        name = "Client zone",
        style = "display: none;"
      ),
      selectInput(
        inputId = NS(id, "selected_zone"),
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
  moduleServer(id, function(input, output, session) {
    # Inspired by https://github.com/rpodcast/shinycal.
    observe({
      tz <- input$client_zone
      if (is.null(tz) || !tz %in% OlsonNames()) tz <- NULL
      updateSelectInput(
        inputId = "selected_zone",
        selected = tz
      )
    })
  })
}
