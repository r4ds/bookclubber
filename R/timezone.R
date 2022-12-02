# I think I need to make this one an actual module to make it work.

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
        choices = OlsonNames()
      )
    )
  )
}

.timezone_server <- function(id = "timezone") {
  shiny::moduleServer(id, function(input, output, session) {
    # Update the timezone dropdown to use the detected zone by default. This is
    # inspired by code from https://github.com/rpodcast/shinycal.
    shiny::observe({
      tz <- input$client_zone
      # If it's blank or malformed, don't select anything by default.
      if (is.null(tz) || !tz %in% OlsonNames()) tz <- character(0)
      shiny::updateSelectInput(
        inputId = "selected_zone",
        selected = tz
      )
    })
  })
}
