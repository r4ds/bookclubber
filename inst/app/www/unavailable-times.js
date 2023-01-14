$(function() {
  /* 
    Solves click issue.
  */
  // Do not 'check' null (no value) checkboxes when clicked
  $('.noValue').on('click', () => false);

  /*
    Overkill way to solve issue when
    space or enter is pressed after focusing 
    on some cell(s) with null value,
    even in multiselection case.
  */
  // For every handsontable table in the site,
  // when any key is pressed into some
  // handsontable table, convert its cells
  // with value null into 'readOnly' type.
  Handsontable.hooks.add('beforeKeyDown', function (e) {
    var hot = this;
    var data = hot.params.data;
    hot.updateSettings({
      cells(row, col) {
        const cellProperties = {};
              
        if (data[row][col] === null) {
          cellProperties.readOnly = true;
        }

        return cellProperties;
      }
    });
  });
});
