.submit_ui <- function(id = "submit") {
  shiny::tagList(
    shiny::p(shiny::strong("Finish by submitting")),
    shiny::actionButton(inputId = "submit", label = "Submit")
  )
}
