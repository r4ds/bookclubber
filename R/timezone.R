#' UI to select a timezone
#'
#' @inheritParams .shared-parameters
#'
#' @return A [shiny::tagList()] that adds the appropriate info to the app for
#'   timezone settings.
#' @keywords internal
.timezone_ui <- function(id = "timezone") {
  return(
    tagList(
      # Add an invisible input to the page to store the JS-detected timezone.
      tags$input(
        type = "text",
        id = NS(id, "client_zone"),
        name = "Client zone",
        style = "display: none;"
      ),
      # Add Javascript to detect the user timezone and fill the input.
      .timezone_ui_javascript(),
      # The user can override the detected timezone (or fill one in if JS
      # fails). The available choices can be overridden in the server.
      selectInput(
        inputId = NS(id, "selected_zone"),
        label = "Select your timezone",
        choices = c(
          "...detecting timezone..." = "",
          tzdb::tzdb_names()
        )
      )
    )
  )
}

.timezone_ui_javascript <- function() {
  htmltools::htmlDependency(
    name = "timezone_detect",
    version = "1.0.0",
    src = "js",
    package = "bookclubber",
    script = "timezone_detect.js"
  )
}

#' A module for setting timezones.
#'
#' @inheritParams .shared-parameters
#' @param allowed_zones A character vector of allowed timezones.
#'
#' @return The selected timezone as a reactive.
#' @keywords internal.
.timezone_server <- function(id = "timezone",
                             allowed_zones = tzdb::tzdb_names()) {
  moduleServer(id, function(input, output, session) {
    # Inspired by https://github.com/rpodcast/shinycal.
    observe({
      tz <- input$client_zone
      if (is.null(tz) || !tz %in% allowed_zones) tz <- NULL
      updateSelectInput(
        inputId = "selected_zone",
        choices = c(
          "PLEASE SELECT A TIMEZONE" = "",
          allowed_zones
        ),
        selected = tz
      )
    })
    reactive(input$selected_zone)
  })
}
