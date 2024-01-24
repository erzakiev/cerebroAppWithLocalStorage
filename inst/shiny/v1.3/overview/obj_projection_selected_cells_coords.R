##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells_coords <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    overview_projection_parameters_plot(),
    overview_projection_data()
  )
  # message('--> trigger "expression_projection_selected_cells"')
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (
    is.null(plotly::event_data("plotly_brushing", source = "overview_projection")) ||
    length(plotly::event_data("plotly_brushing", source = "overview_projection")) == 0
  ) {
    return(NULL)
    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    selected_cells_coords <- plotly::event_data("plotly_brushing", source = "overview_projection")
    # message(str(selected_cells))
    return(selected_cells_coords)
  }
})
