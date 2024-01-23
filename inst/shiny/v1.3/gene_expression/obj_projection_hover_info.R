##----------------------------------------------------------------------------##
## Hover info for cells in projection.
##----------------------------------------------------------------------------##
expression_projection_hover_info <- reactive({
  req(
    hover_info_projections(),
    expression_projection_cells_to_show()
  )
  # message('--> trigger "expression_projection_hover_info"')
  exprsns <- expression_projection_expression_levels()
  if (
    !is.null(preferences[["show_hover_info_in_projections"]]) &&
    preferences[['show_hover_info_in_projections']] == TRUE
  ) {
    exprsns <- exprsns[expression_projection_cells_to_show()]
    hover_info <- hover_info_projections()[expression_projection_cells_to_show()]
    hover_info <- glue::glue(
      "{hover_info}<br>",
      "<b>Expression level</b>: {formatC(exprsns, format = 'f', big.mark = ',', digits = 3)}"
    )
  } else {
    hover_info <- hover_info_projections()
    hover_info <- glue::glue(
      "{hover_info}<br>",
      "<b>Expression level</b>: {formatC(exprsns, format = 'f', big.mark = ',', digits = 3)}"
    )
  }
  # message(str(hover_info))
  return(hover_info)
})
