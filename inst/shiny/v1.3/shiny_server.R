##----------------------------------------------------------------------------##
## Server function for Cerebro.
##----------------------------------------------------------------------------##
launch.browser = F
#options(shiny.port = 3838) 
library(shinyFiles)

server <- function(input, output, session) {
  
  ##--------------------------------------------------------------------------##
  ## Load color setup, plotting and utility functions.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_setup.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/plotting_functions.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/utility_functions.R"), local = TRUE)

  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  preferences <- reactiveValues(
    scatter_plot_point_size = list(
      min = 1,
      max = 20,
      step = 1,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_point_size']]),
        Cerebro.options[['projections_default_point_size']],
        2
      )
    ),
    scatter_plot_point_opacity = list(
      min = 0.1,
      max = 1.0,
      step = 0.1,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_point_opacity']]),
        Cerebro.options[['projections_default_point_opacity']],
        1.0
      )
    ),
    scatter_plot_percentage_cells_to_show = list(
      min = 10,
      max = 100,
      step = 10,
      default = ifelse(
        exists('Cerebro.options') &&
        !is.null(Cerebro.options[['projections_default_percentage_cells_to_show']]),
        Cerebro.options[['projections_default_percentage_cells_to_show']],
        100
      )
    ),
    use_webgl = TRUE,
    show_hover_info_in_projections = ifelse(
      exists('Cerebro.options') &&
      !is.null(Cerebro.options[['projections_show_hover_info']]),
      Cerebro.options[['projections_show_hover_info']],
      TRUE
    )
  )

  ## paths for storing plots
  available_storage_volumes <- c(
    Home = "~",
    shinyFiles::getVolumes()()
  )

  ##--------------------------------------------------------------------------##
  ## Load data set.
  ##--------------------------------------------------------------------------##

  ## reactive value holding path to file of data set to load
  data_to_load <- reactiveValues()
  prefix <- 'D:/CerebroData/'
  
  files_selected <- reactive({
    shinyFileChoose(
      input, 
      "input_file2", 
      roots = c(home = prefix),
      filetypes = c('crb'),
    )
    return(parseFilePaths(c(home = prefix), input$input_file2)$datapath)
  })

  ## listen to selected 'input_file', initialize before UI element is loaded
  observeEvent(c(input[['input_file']], input[['input_file2']]), ignoreNULL = FALSE, {
    path_to_load <- ''
    #print(input[['input_file2']])
    print('line 92 ok')
    print("printing files_selected()")
    print(files_selected())
    print("printing file.exists(files_selected())")
    print(file.exists(files_selected()))
    print('line 97 ok')
    
    ## grab path from 'input_file' if one is specified
    if (
      !is.null(input[["input_file"]]) &&
      !is.na(input[["input_file"]]$datapath) &&
      file.exists(input[["input_file"]]$datapath)
    ) {
      #print("line 81 ok")
      #print(input[["input_file"]])
      path_to_load <- input[["input_file"]]$datapath
      file.copy(from = path_to_load, to = paste0(prefix,input[["input_file"]]$name), overwrite = T)
      ## take path or object from 'Cerebro.options' if it is set and points to an
      ## existing file or object
    } else if (
      exists('Cerebro.options') &&
      !is.null(Cerebro.options[["crb_file_to_load"]])
    ) {
      print("line 89 ok")
      file_to_load <- Cerebro.options[["crb_file_to_load"]]
      if (file.exists(file_to_load) || exists(file_to_load)) {
        path_to_load <- .GlobalEnv$Cerebro.options$crb_file_to_load
      }
    } else if(length(files_selected())>0){
      if(
      !is.null(files_selected()) &&
      file.exists(files_selected())
    ){
      toPath <- files_selected()
      print(toPath)
      print("line 100 ok")
      path_to_load <- toPath
      }
    }
    ## assign path to example file if none of the above apply
    if (path_to_load=='') {
      print("line 105 ok")
      path_to_load <- system.file("extdata/v1.3/example.crb", package = "cerebroApp")
    }
    print("line 108 ok")
    ## set reactive value to new file path
    data_to_load$path <- path_to_load
  })

  ## create reactive value holding the current data set
  data_set <- reactive({
    dataset_to_load <- data_to_load$path
    if (exists(dataset_to_load)) {
      print(glue::glue("[{Sys.time()}] Load from variable: {dataset_to_load}"))
      data <- get(dataset_to_load)
    } else {
      ## log message
      print(glue::glue("[{Sys.time()}] File to load: {dataset_to_load}"))
      ## read the file
      data <- readRDS(dataset_to_load)
    }
    ## log message
    message(data$print())
    ## check if 'expression' slot exists and print log message with its format
    ## if it does
    if ( !is.null(data$expression) ) {
      print(glue::glue("[{Sys.time()}] Format of expression data: {class(data$expression)}"))
    }
    ## return loaded data
    return(data)
  })

  # list of available trajectories
  available_trajectories <- reactive({
    req(!is.null(data_set()))
    ## collect available trajectories across all methods and create selectable
    ## options
    available_trajectories <- c()
    available_trajectory_method <- getMethodsForTrajectories()
    ## check if at least 1 trajectory method exists
    if ( length(available_trajectory_method) > 0 ) {
      ## cycle through trajectory methods
      for ( i in seq_along(available_trajectory_method) ) {
        ## get current method and names of trajectories for this method
        current_method <- available_trajectory_method[i]
        available_trajectories_for_this_method <- getNamesOfTrajectories(current_method)
        ## check if at least 1 trajectory is available for this method
        if ( length(available_trajectories_for_this_method) > 0 ) {
          ## cycle through trajectories for this method
          for ( j in seq_along(available_trajectories_for_this_method) ) {
            ## create selectable combination of method and trajectory name and add
            ## it to the available trajectories
            current_trajectory <- available_trajectories_for_this_method[j]
            available_trajectories <- c(
              available_trajectories,
              glue::glue("{current_method} // {current_trajectory}")
            )
          }
        }
      }
    }
    # message(str(available_trajectories))
    return(available_trajectories)
  })

  # hover info for projection
  hover_info_projections <- reactive({
    # message('--> trigger "hover_info_projections"')
    if (
      !is.null(preferences[["show_hover_info_in_projections"]]) &&
      preferences[['show_hover_info_in_projections']] == TRUE
    ) {
      cells_df <- getMetaData()
      hover_info <- buildHoverInfoForProjections(cells_df)
      hover_info <- setNames(hover_info, cells_df$cell_barcode)
    } else {
      hover_info <- 'none'
    }
    # message(str(hover_info))
    return(hover_info)
  })

  ##--------------------------------------------------------------------------##
  ## Show "Trajectory" tab if there are trajectories in the data set.
  ##--------------------------------------------------------------------------##

  ## the tab item needs to be in the `output`
  output[["sidebar_item_trajectory"]] <- renderMenu({
    req(!is.null(data_set()))
    menuItem("Trajectory", tabName = "trajectory", icon = icon("random"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_trajectory_tab <- reactive({
    req(!is.null(data_set()))
    ## if at least one trajectory is present, return TRUE, otherwise FALSE
    if (
      !is.null(getMethodsForTrajectories()) &&
      length(getMethodsForTrajectories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  ## listen to reactive value defined above and toggle visibility of trajectory
  ## tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_trajectory",
      condition = show_trajectory_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Show "Extra material" tab if there is some extra material in the data set.
  ##--------------------------------------------------------------------------##

  ## the tab item needs to be in the `output`
  output[["sidebar_item_extra_material"]] <- renderMenu({
    ## require a data set to be loaded
    req(!is.null(data_set()))
    menuItem("Extra material", tabName = "extra_material", icon = icon("gift"))
  })

  ## this reactive value checks whether the tab should be shown or not
  show_extra_material_tab <- reactive({
    ## require a data set to be loaded
    req(!is.null(data_set()))
    ## if at least one piece of extra material is present, return TRUE,
    ## otherwise FALSE
    if (
      !is.null(getExtraMaterialCategories()) &&
      length(getExtraMaterialCategories()) > 0
    ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  ## listen to reactive value defined above and toggle visibility of extra
  ## material tab accordingly
  observe({
    shinyjs::toggleElement(
      id = "sidebar_item_extra_material",
      condition = show_extra_material_tab()
    )
  })

  ##--------------------------------------------------------------------------##
  ## Print log message when switching tab (for debugging).
  ##--------------------------------------------------------------------------##
  observe({
    print(glue::glue("[{Sys.time()}] Active tab: {input[['sidebar']]}"))
  })

  ##--------------------------------------------------------------------------##
  ## Tabs.
  ##--------------------------------------------------------------------------##
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/load_data/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/overview/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/groups/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/most_expressed_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/marker_genes/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/enriched_pathways/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_expression/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/trajectory/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/extra_material/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/gene_id_conversion/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/analysis_info/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/color_management/server.R"), local = TRUE)
  source(paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.3/about/server.R"), local = TRUE)
  
  session$onSessionEnded(function() {
    print('End of session')
    session$close()
  })
  
}
