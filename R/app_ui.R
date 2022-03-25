#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @keywords internal
.app_ui <- function(request) {
  shiny::tagList(
    # This function loads things like javascript scripts or CSS.
    .golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      # Application title
      shiny::titlePanel("R4DS Book Club Planner"),

      shiny::fluidRow(
        # Add HTML to the page to store the timezone.
        shiny::HTML(
          '<input type="text" id="clientZone" name="Client zone" style="display: none;"> '
        ),
        shiny::column(
          width = 3,
          shiny::htmlOutput(outputId = "username")
        ),
        shiny::column(
          width = 3, #offset = 1,
          shiny::selectInput(
            inputId = "bookname",
            label = "Select Book",
            choices = approved_books
          )
        ),
        shiny::column(
          width = 3, #offset = 1,
          shiny::selectInput(
            inputId = "timezone",
            label = "Select Your Time Zone",
            choices = OlsonNames()
          )
        ),
        shiny::column(
          width = 2,
          offset = 1,
          shiny::p(shiny::strong("Finish by submitting")),
          shiny::actionButton(inputId = "submit", label = "Submit")
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            width = 5,
            offset = 1,
            shiny::h4("Select your availability"),
            rhandsontable::rHandsontableOutput("time_table")
          ),
          shiny::column(
            width = 6,
            shiny::h4("Your availability selections"),
            shiny::tableOutput("selected")
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
#' @keywords internal
.golem_add_external_resources <- function(){

  golem::add_resource_path(
    'www', .app_sys('app/www')
  )

  shiny::tags$head(
    # favicon(),
    golem::bundle_resources(
      path = .app_sys('app/www'),
      app_title = 'bookclubber'
    )
  )
}

