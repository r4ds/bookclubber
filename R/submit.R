.submit_ui <- function(id = "submit") {
  tagList(
    p(strong("Finish by submitting")),
    actionButton(inputId = "submit", label = "Submit")
  )
}
