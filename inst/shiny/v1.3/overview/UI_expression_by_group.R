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
        boxTitle("Expression levels by group in selection"),
        cerebroInfoButton("expression_by_group_info")
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
  )
  
  ## check if user requested to show expression in separate panels
  ## ... separate panels requested and "gene" column present (which means
  ##     expression was actually split by gene)
  ##     don't plot anything because data is not present
  ##     even if I merged all meta data in the data frame, it wouldn't be correct
  ##     because cells are plotted once per gene
  cells_df <- getMetaData()
  color_variable <- input[["overview_selected_cells_plot_select_variable"]]
  
  if (
    is.factor(cells_df[[ color_variable ]]) ||
    is.character(cells_df[[ color_variable ]])
  ){
    ## variable is categorical, can proceed
    
    ## prepare plot
    print('printing input[[overview_by_group_selected_group]] from UI_expression_by_group')
    print(input[["overview_by_group_selected_group"]])
    
    print('printing color_variable from UI_expression_by_group')
    print(color_variable)
    
    saveRDS(input[["overview_by_group_selected_group"]], file = 'input_overview_by_group_selected_group.RDS')
    saveRDS(color_variable, file = 'color_variable.RDS')
    saveRDS(object = cells_df, file = 'cells_df.RDS', compress = T)
    cells_df %>%
      plotly::plot_ly(
        x = ~cells_df[[ input[["overview_by_group_selected_group"]] ]],
        y = ~cells_df[[color_variable]],
        split = ~cells_df[[ input[["overview_by_group_selected_group"]] ]],
        type = "violin",
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        ),
        color = ~.[[ input[["overview_by_group_selected_group"]] ]],
        colors = reactive_colors()[[ input[["overview_by_group_selected_group"]] ]],
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
          range = c(0, max(cells_df$level, na.rm = TRUE) * 1.2),
          hoverformat = ".2f",
          mirror = TRUE,
          showline = TRUE
        ),
        dragmode = "select",
        hovermode = "compare"
      )
  }
})
