test_that("Book UI has expectd form", {
  expect_snapshot(.book_ui())
})

test_that("Book server returns selected book", {
  expect_warning(
    testServer(.book_server, {
      session$setInputs(selected_book = "R for Data Science")
      book <- session$getReturned()
      expect_identical(
        book(),
        "R for Data Science"
      )
    }),
    "MockShinySession"
  )
})

# As far as I can find through manual testing, shinytest2 can't "see" query
# changes in the URL. Test manually to validate that part.
