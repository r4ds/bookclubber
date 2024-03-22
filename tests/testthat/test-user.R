# Most or all of this should eventually move to r4ds/shinyslack.

test_that("user UI has expectd form", {
  expect_snapshot(.user_ui())
})

test_that("user server builds expected output", {
  local_mocked_bindings(
    .shinyslack_user_info = function(components) {
      # Return a function to simulate a reactive.
      function() {
        list(display_name = "test_user", user_id = "test_user_id")
      }
    }
  )
  testServer(.user_server, expect_snapshot(output$user_name))
})

test_that("user server returns expected object", {
  test_user_info <- list(user_id = "test_user_id", user_name = "test_user")
  local_mocked_bindings(
    .shinyslack_user_info = function(components) {
      # Return a function to simulate a reactive.
      function() test_user_info
    }
  )
  testServer(.user_server, {
    slack_user_info <- session$getReturned()
    expect_identical(
      slack_user_info(),
      test_user_info
    )
  })
})
