#' UI to Choose a Book
#'
#' @inheritParams .shared-parameters
#'
#' @return A shiny module to select a book.
#' @keywords internal
.book_ui <- function(id = "book_name") {
  # In a future update, this will become a proper module.
  return(
    shiny::selectInput(
      inputId = id,
      label = "Select Book",
      choices = c("...loading..." = "")
    )
  )
}

#' Load Books and Set Dropdown Values
#'
#' @inheritParams .shared-parameters
#' @param approved_books A data.frame of approved books with column `book_name`.
#'
#' @return An observer that sets up the book drop-down menu.
#' @keywords internal
.book_observer <- function(id = "book_name",
                           approved_books,
                           session = shiny::getDefaultReactiveDomain()) {
  # This should be turned into a module.

  # Return an observer that sets the book drop-down. We still want to SHOW them
  # all of the options, though, so they can choose another book if they'd like.
  return(
    shiny::observe(
      {
        book_choices <- c(
          "PLEASE SELECT A BOOK" = "",
          approved_books$book_name
        )

        query <- shiny::parseQueryString(session$clientData$url_search)

        if (
          !is.null(query[["bookname"]]) &&
          query[["bookname"]] %in% book_choices
        ) {
          shiny::updateSelectInput(
            session,
            id,
            label = "Book Selected",
            choices = book_choices,
            selected = query[["bookname"]]
          )
        } else {
          shiny::updateSelectInput(
            session,
            id,
            choices = book_choices,
            selected = NULL
          )
        }
      }
    )
  )
}
