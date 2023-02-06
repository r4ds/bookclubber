#' UI to Choose a Book
#'
#' @return A shiny module to select a book.
#' @keywords internal
.book_ui <- function(id = "book_name") {
  # In a future update, this will become a proper module.
  return(
    shiny::selectInput(
      inputId = id,
      label = "Select Book",
      choices = "...loading..."
    )
  )
}

#' Load Books and Set Dropdown Values
#'
#' @param session The shiny session. I don't know of a case where we shouldn't
#'   use the default.
#'
#' @return An observer that sets up the book drop-down menu.
#' @keywords internal
.book_server <- function(id = "book_name",
                         session = shiny::getDefaultReactiveDomain()) {
  # This should be turned into a module. The UI should become static if the book
  # is set via url.

  # We don't need the approved book list to change while they're in here.
  .approved_books <- shiny::isolate(.approved_books())

  # Return an observer that sets the book drop-down. I don't think this actually
  # needs to be an observer, it should just happen onload.
  return(
    shiny::observe(
      {
        book_choices <- c(
          "PLEASE SELECT A BOOK",
          .approved_books$book_name
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
            choices = query[["bookname"]],
            selected = query[["bookname"]]
          )
        } else {
          shiny::updateSelectInput(
            session,
            id,
            choices = book_choices,
            selected = "PLEASE SELECT A BOOK"
          )
        }
      }
    )
  )
}

#' Load Books
#'
#' @return A tibble with the valid books.
#' @keywords internal
.load_books <- function() {
  return(
    dplyr::arrange(
      .read_gs4(
        sheet = "Approved Books",
        # Select a couple extra columns to avoid issues with rearrangements.
        range = "A:D",
        col_types = "cccc"
      ),
      tolower(.data$book_name)
    )
  )
}
