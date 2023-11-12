# Book UI has expectd form

    Code
      .book_ui()
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="book-selected_book-label" for="book-selected_book">Select a book</label>
        <div>
          <select id="book-selected_book" class="shiny-input-select"><option value="" selected>...loading...</option></select>
          <script type="application/json" data-for="book-selected_book">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>

