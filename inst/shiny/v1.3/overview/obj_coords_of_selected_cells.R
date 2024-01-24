##----------------------------------------------------------------------------##
## Text showing the coordinates of selected cells.
##----------------------------------------------------------------------------##
output[["overview_coords_of_selected_cells"]] <- renderText({
  ## check selection
  #print('entered the overview_coords_of_selected_cells calculation')
  ## ... selection has not been made or there is no cell in it
  if ( is.null(overview_projection_selected_cells()) ) {
    ## manually set counter to 0
    return(NULL)
    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    coords_of_selected_cells <- overview_projection_selected_cells()# %>%
    #  nrow() %>%
    #  formatC(format = "f", big.mark = ",", digits = 0)
  }
  ## prepare string to show
  
  p1 <- paste0("<b>Margins of the current selection</b>: <br>x: ", paste0(round(coords_of_selected_cells$x, 3), collapse = ' '))
  p2 <- paste0("<br>y: ", paste0(round(coords_of_selected_cells$y, 3), collapse = ' '))
  return(paste0(p1, p2))
})
