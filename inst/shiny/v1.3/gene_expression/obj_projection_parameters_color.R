##----------------------------------------------------------------------------##
## Collect color parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_color <- reactive({
  ## require input UI elements
  req(
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_range"]],
    input[["expression_projection_color_range_quantiles"]]
  )
  
  # message('--> trigger "expression_projection_parameters_color"')
  ## collect parameters
  
  parameters <- list(
    color_scale = input[["expression_projection_color_scale"]],
    color_range = input[["expression_projection_color_range"]]
  )
  
  if(isTRUE(as.logical(input[["expression_projection_color_range_quantiled"]]))){
    parameters <- list(
      color_scale = input[["expression_projection_color_scale"]],
      color_range = input[["expression_projection_color_range_quantiles"]]
    )
  }
  # message(str(parameters))
  return(parameters)
})
