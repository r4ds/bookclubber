#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # Application title
      titlePanel("R4DS Book Club Planner"),

      fluidRow(
        column(
          width = 3,
          shiny::textInput(inputId = "username", label = "Name", value = "")
        ),
        column(
          width = 3, #offset = 1,
          shiny::selectInput(
            inputId = "bookname",
            label = "Select Book",
            choices = approved_books
          )
        ),
        column(
          width = 3, #offset = 1,
          shiny::selectInput(
            inputId = "timezone",
            label = "Select Your Time Zone",
            choices = OlsonNames()
          )
        ),
        column(
          width = 2,
          offset = 1,
          p(strong("Finish by submitting")),
          shiny::actionButton(inputId = "submit", label = "Submit")
        ),
        hr(),
        fluidRow(
          column(
            width = 5,
            offset = 1,
            h4("Select your availability"),
            rhandsontable::rHandsontableOutput("time_table")
          ),
          column(
            width = 6,
            h4("Your availability selections"),
            tableOutput("selected")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'bookclubber'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

