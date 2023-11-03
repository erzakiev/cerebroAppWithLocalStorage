##----------------------------------------------------------------------------##
## Select group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select which group should be shown.
##----------------------------------------------------------------------------##
output[["groups_select_group_UI"]] <- renderUI({
  tagList(
    div(
      HTML('<h3 style="text-align: center; margin-top: 0"><strong>Choose a grouping variable:</strong></h2>')
    ),
    fluidRow(
      column(2),
      column(8,
        selectInput(
          "groups_selected_group",
          label = NULL,
          choices = getGroups(),
          width = "100%"
        )
      ),
      column(2)
    )
  )
})

output[["excludedGroupsSankeyA_UI"]] <- renderUI({
selectizeInput(
  'excludedGroupsSankeyA',
  label = 'Initial Points To Remove',
  choices = data.table::as.data.table(data.frame("Groups" = getGroupLevels(input$groups_selected_group))),
  multiple = TRUE,
  options = list(
    create = F
  )
)
}
)
output[["excludedGroupsSankeyB_UI"]] <- renderUI({
  selectizeInput(
    'excludedGroupsSankeyB',
    label = 'Initial Points To Remove',
    choices = data.table::as.data.table(data.frame("Groups" = getGroupLevels(input$groups_by_other_group_second_group))),
    multiple = TRUE,
    options = list(
      create = F
    )
  )
}
)
