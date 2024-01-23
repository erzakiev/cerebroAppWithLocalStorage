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
  print('printing head(exprsns)')
  print(head(exprsns))
  if (
    !is.null(preferences[["show_hover_info_in_projections"]]) &&
    preferences[['show_hover_info_in_projections']] == TRUE
  ) {
    toShow <- expression_projection_cells_to_show()
    print('printing again head(toShow)')
    print(head(toShow))
    #exprsns <- exprsns[toShow]
    print('printing again head(exprsns)')
    print(head(exprsns))
    hover_info <- hover_info_projections()[toShow]
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
  print('printing head of hover_info')
  print(head(hover_info))
  # message(str(hover_info))
  return(hover_info)
})
