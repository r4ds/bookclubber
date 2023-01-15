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

  /*
    When pressing Tab multiple times to change selected
    cell in table (while not keeping pressed Shift) 
    do not select the cells with null value,
    such as the cells for unavailable times.
  */
  Handsontable.hooks.add('beforeKeyDown', function (event) {
    if(!event.shiftKey && event.key === 'Tab') { 
      var hot = this;
      var numberOfRows = hot.countRows();
      var numberOfColumns = hot.countCols();
  
      // Retrieve selected cell's indices (position) 
      var row = hot.getSelected()[0][0];
      var col = hot.getSelected()[0][1];
  
      // Select 'closest next' cell with 
      // non-null data, in case it exists
      var cellData = null;
      while (cellData === null) {
        if (col + 1 < numberOfColumns) {
          col = col + 1;
        } else if (row + 1 < numberOfRows) {
          col = 0;
          row = row + 1;
        } else {
          // Selected cell is the last one 
          // (right lower corner of table)
          return;
        }
        cellData = hot.getDataAtCell(row, col);
      }
      // setTimeout will be used in order to select
      // a cell after the default behaviour after
      // pressing Tab has occured.
      setTimeout(() => hot.selectCell(row, col), 10);
    }
  });

  /*
    When pressing Tab multiple times to change selected
    cell in table (while keeping pressed Shift) 
    do not select the cells with null value,
    such as the cells for unavailable times.
  */
  Handsontable.hooks.add('beforeKeyDown', function (event) {
    if (event.shiftKey && event.key === 'Tab') {
      var hot = this;
      var numberOfColumns = hot.countCols();
  
      // Retrieve selected cell's indices (position) 
      var row = hot.getSelected()[0][0];
      var col = hot.getSelected()[0][1];
  
      // Select 'closest previous' cell with 
      // non-null data, in case it exists
      var cellData = null;
      while (cellData === null) {
        if (col > 0) {
          col = col - 1;
        } else if (row > 0) {
          col = numberOfColumns - 1;
          row = row - 1;
        } else {
          // Selected cell is the first one 
          // (left upper corner of table)
          return;
        }
        cellData = hot.getDataAtCell(row, col);
      }
      setTimeout(() => hot.selectCell(row, col), 10);
    }
  });
});
