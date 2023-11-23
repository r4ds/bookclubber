#' UI to Choose a Book
#'
#' @inheritParams .shared-parameters
#'
#' @return A shiny module to select a book.
#' @keywords internal
.book_ui <- function(id = "book") {
  return(
    selectInput(
      inputId = NS(id, "selected_book"),
      label = "Select a book",
      choices = c("...loading..." = "")
    )
  )
}

#' Book module server
#'
#' @inheritParams .shared-parameters
#'
#' @return The selected book as a reactive.
#' @keywords internal
.book_server <- function(id = "book") {
  book_choices <- .book_get_choices()
  moduleServer(id, function(input, output, session) {
    # I can't find a way to get a non-reactive version of the query string.
    query_book <- reactive({
      query <- getQueryString()
      query_book <- book_choices[book_choices == query$bookname]
    })

    # Only use the query_string to update the input when the app initially loads
    # AND THE UI INCLUDES THE INPUT. After that, the input is the source of
    # truth.
    observeEvent(
      query_book(),
      {
        if (
          length(query_book()) && length(input$selected_book) &&
          query_book() != input$selected_book
        ) {
          updateSelectInput( # nocov start (can't find a way to automate)
            session,
            "selected_book",
            label = "Book Selected",
            choices = book_choices,
            selected = query_book()
          ) # nocov end
        } else {
          updateSelectInput(
            session,
            "selected_book",
            label = "Book Selected",
            choices = book_choices,
            selected = NULL
          )
        }
      },
      ignoreNULL = FALSE,
      once = TRUE
    )

    observeEvent(
      input$selected_book != "",
      { # nocov start (Can't find a way to automate)
        # Get the CURRENT query string, not necessarily the same as when the app
        # loaded.
        query_string <- getQueryString()
        query_string$bookname <- input$selected_book
        query_string <- paste0(
          "?",
          paste(names(query_string), query_string, sep = "="),
          collapse = "&"
        )
        updateQueryString(query_string)
      }, # nocov end
      ignoreInit = TRUE
    )

    return(reactive(input$selected_book))
  })
}

# Abstract for potential mocking.
.book_get_choices <- function() {
  approved_books <- bookclubdata::approved_books(refresh = TRUE)
  c("PLEASE SELECT A BOOK" = "", approved_books$book_name)
}
