##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["expression_projection_selected_cells_coords"]] <- renderText({
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if ( is.null(expression_projection_selected_cells_coords()) ) {
    ## return empty line
    return(NULL)
    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    coords_of_selected_cells <- expression_projection_selected_cells_coords() #%>%
      #formatC(format = "f", big.mark = ",", digits = 0)
  }
  ## prepare string to show
  
  
  paste0("<b>Coordinates of the current selection</b>: ", coords_of_selected_cells)
})
