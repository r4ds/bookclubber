test_that("timezone UI has expectd form", {
  expect_snapshot(.timezone_ui())
})

test_that("timezone server returns selected zone", {
  testServer(.timezone_server, {
    session$setInputs(selected_zone = "America/Chicago")
    timezone <- session$getReturned()
    expect_identical(
      timezone(),
      "America/Chicago"
    )
  })
})

test_that("timezone module uses detected timezone", {
  skip_on_cran()
  timezone_app <- function() {
    shinyApp(
      ui = fluidPage(
        .timezone_ui(),
        textOutput("selected_zone")
      ),
      server = function(input, output, session) {
        timezone <- .timezone_server()
        output$selected_zone <- renderText(timezone())
      }
    )
  }
  app <- AppDriver$new(timezone_app(), name = "timezone")
  app$set_inputs(`timezone-client_zone` = "Australia/Lord_Howe")
  app$expect_values()
  app$set_inputs(`timezone-selected_zone` = "America/New_York")
  app$expect_values()
})
