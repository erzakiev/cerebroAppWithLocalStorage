##----------------------------------------------------------------------------##
## Tab: Overview
##
## Expression by group
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with input selection (which group to show) and plot.
##----------------------------------------------------------------------------##
output[["expression_by_group_overview_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression levels by group in selection")
      ),
      tagList(
        selectInput(
          "overview_by_group_selected_group",
          label = "Select a group to show expression by:",
          choices = getGroups(),
          width = "100%"
        ),
        plotly::plotlyOutput("expression_by_group_overview")
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Violin/box plot.
##----------------------------------------------------------------------------##

output[["expression_by_group_overview"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_by_group_selected_group"]],
    input[['overview_projection_point_color']]
  )
  
  ## check if user requested to show expression in separate panels
  ## ... separate panels requested and "gene" column present (which means
  ##     expression was actually split by gene)
  ##     don't plot anything because data is not present
  ##     even if I merged all meta data in the data frame, it wouldn't be correct
  ##     because cells are plotted once per gene
  cells_df <- getMetaData()
  y_variable <- input[['overview_projection_point_color']]
  x_variable <- input[["overview_by_group_selected_group"]]
  
  print('printing x_variable')
  print(x_variable)
  
  print('printing y_variable')
  print(y_variable)
  
  #saveRDS(object = cells_df, file = 'cells_df.RDS', compress = T)
  #saveRDS(reactive_colors(), file = 'reactive_colors.RDS')
  
  if (
    is.factor(cells_df[[y_variable]]) ||
    is.character(cells_df[[y_variable]])
  ){} else {
    ## variable isn't categorical, proceed
    
    ## prepare plot
    cells_df %>%
      plotly::plot_ly(
        x = ~.data[[x_variable]],
        y = ~.data[[y_variable]],
        split = ~.data[[x_variable]],
        type = "violin",
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        ),
        color = ~.data[[x_variable]],
        colors = reactive_colors()[[ x_variable ]],
        source = "subset",
        showlegend = FALSE,
        hoverinfo = "y",
        marker = list(
          size = 5
        )
      ) %>%
      plotly::layout(
        title = "",
        xaxis = list(
          title = "",
          mirror = TRUE,
          showline = TRUE
        ),
        yaxis = list(
          title = "Expression level",
          range = c(0, max(cells_df[[y_variable]], na.rm = TRUE) * 1.2),
          hoverformat = ".2f",
          mirror = TRUE,
          showline = TRUE
        ),
        dragmode = "select",
        hovermode = "compare"
      )
    }
})
