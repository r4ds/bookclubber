# user UI has expectd form

    Code
      .user_ui()
    Output
      <div id="user_name-user_name" class="shiny-html-output"></div>

# user server builds expected output

    Code
      output$user_name
    Output
      [1] "<strong>Logged in as</strong> <br/> test_user"

