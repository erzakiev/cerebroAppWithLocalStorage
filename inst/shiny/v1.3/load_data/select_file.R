##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Cerebro.
##----------------------------------------------------------------------------##

existing_files <- list.files(path = "/media/minicluster/Data/CerebroData", pattern = '.crb', full.names = T)

output[["load_data_select_file_UI"]] <- renderUI({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[['mode']]) &&
    Cerebro.options[["mode"]] != "closed"
  ) {
    tagList(
      fluidRow(
        htmlOutput("load_data_mode_open")
      ),
      fluidRow(
        column(6,
          titlePanel("Load data"),
          fileInput(
            inputId = "input_file",
            label = "Select input data (.crb file)",
            multiple = FALSE,
            accept = c(".crb",".cerebro"),
            width = '350px',
            buttonLabel = "Browse your local system...",
            placeholder = "Bugs out in Chrome occasionally"
          )
        ),
        column(6, 
               titlePanel("... or select one of the cached files"),
               #selectizeInput(inputId = "input_file2", '.crb.files', choices=basename(existing_files)), options = list(
               #   placeholder = 'Please select an option below',
               #  onInitialize = I('function() { this.setValue(""); }')
               #)#,
               #selectInput("input_file2", ".crb files", choices = basename(existing_files), selected='example.crb')
               shinyFiles::shinyFilesButton(id = 'input_file2', label = '.crb files', title = 'Please select one of the cached .crb files', multiple = F)
        )
      )
    )
  } else {
    fluidRow(
      htmlOutput("load_data_mode_closed")
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "open" mode.
##----------------------------------------------------------------------------##

output[["load_data_mode_open"]] <- renderText({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Please load your data set or take a look at the pre-loaded data.</p>"
    )
  }
})

##----------------------------------------------------------------------------##
## Text message if Cerebro was launched in "closed" mode.
##----------------------------------------------------------------------------##

output[["load_data_mode_closed"]] <- renderText({
  if (
    exists('Cerebro.options') &&
    !is.null(Cerebro.options[["welcome_message"]])
  ) {
    HTML(Cerebro.options[["welcome_message"]])
  } else {
    HTML(
      "<h3 style='text-align: center; margin-top: 0px'><strong>Welcome to Cerebro!</strong></h3>
      <p style='text-align: center'>Cerebro was launched in 'closed' mode, which means you cannot load your own data set. Instead, take a look at the pre-loaded data.</p>
      <br>"
    )
  }
})
