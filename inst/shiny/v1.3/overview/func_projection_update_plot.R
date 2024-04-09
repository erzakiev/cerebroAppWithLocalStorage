##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
overview_projection_update_plot <- function(input) {
  ## assign input data to new variables
  cells_df <- input[['cells_df']]
  saveRDS(cells_df, file = 'cells_df.RDS', compress = T)
  rownames(cells_df) <- cells_df$cell_barcode
  print('printing head(cells_df)')
  print(head(cells_df))
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  plot_parameters <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  hover_info <- input[['hover_info']]
  color_input <- cells_df[[ plot_parameters[['color_variable']] ]]
  selected_cells_df <- overview_projection_selected_cells()
  selected_cells <- selected_cells_df$pointNumber
  selected_cells_barcode <- selected_cells_df$customdata
  saveRDS(selected_cells_df, file = 'overview_projection_selected_cells().RDS', compress = T)
  ## follow this when the coloring variable is numeric
  if ( is.numeric(color_input) ) {
    ## put together meta data
    output_meta <- list(
      color_type = 'continuous',
      traces = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']]
    )
    if(length(selected_cells)==0 | is.null(selected_cells)){
      # do not pass selectedpoints
      output_data <- list(
        x = coordinates[[1]],
        y = coordinates[[2]],
        color = color_input,
        point_size = plot_parameters[["point_size"]],
        point_opacity = plot_parameters[["point_opacity"]],
        point_line = list(),
        x_range = plot_parameters[["x_range"]],
        y_range = plot_parameters[["y_range"]],
        reset_axes = reset_axes,
        identifier = rownames(coordinates)
      )
      
    } else {
      # DO pass selectedpoints
      output_data <- list(
        x = coordinates[[1]],
        y = coordinates[[2]],
        color = color_input,
        point_size = plot_parameters[["point_size"]],
        point_opacity = plot_parameters[["point_opacity"]],
        point_line = list(),
        x_range = plot_parameters[["x_range"]],
        y_range = plot_parameters[["y_range"]],
        reset_axes = reset_axes,
        identifier = rownames(coordinates),
        selectedpoints = selected_cells
      )
    }
    ## put together data
    
    
    #print('printing numerical categorical output_data from overview_projection_update_plot')
    #print(output_data)
    
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    ## put together hover info
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = 'empty'
    )
    if ( plot_parameters[["hover_info"]] ) {
      output_hover[['text']] <- unname(hover_info)
    }
    
    ## send request to update projection to JavaScript functions (2D / 3D)
    saveRDS(list(output_meta, output_data, output_hover), file = 'output4updatePlot2DContinious.RDS', compress = T)
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      shinyjs::js$updatePlot2DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      output_data[['z']] <- coordinates[[3]]
      output_data[['z_range']] <- plot_parameters[["z_range"]]
      shinyjs::js$updatePlot3DContinuous(
        output_meta,
        output_data,
        output_hover
      )
    }
  ## follow this procedure when coloring variable is not numeric
  } else {
    ## put together meta data
    output_meta <- list(
      color_type = 'categorical',
      traces = list(),
      color_variable = plot_parameters[['color_variable']]
    )
    ## put together data
    output_data <- list(
        x = list(),
        y = list(),
        z = list(),
        identifier = list(),
        color = list(),
        point_size = plot_parameters[["point_size"]],
        point_opacity = plot_parameters[["point_opacity"]],
        point_line = list(),
        x_range = plot_parameters[["x_range"]],
        y_range = plot_parameters[["y_range"]],
        z_range = plot_parameters[["z_range"]],
        #selectedpoints = selected_cells,
        reset_axes = reset_axes
    )
    
        
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    ## put together hover info
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = ifelse(plot_parameters[["hover_info"]], list(), 'test')
    )
    ## prepare trace for each group of the categorical coloring variable and
    ## send request to update projection to JavaScript function (2D/3D)
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['identifier']][[i]] <- rownames(coordinates)[cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        print('printing head(cells_to_extract)')
        print(head(cells_to_extract))
        print('printing selected_cells_barcode')
        print(head(selected_cells_barcode))
        intersection <- (which(cells_df[cells_to_extract,1] %in% selected_cells_barcode)-1)
        print('printing intersection')
        print(intersection)
        output_data[['selectedpoints']][[i]] <- intersection
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(coordinates, cells_df, 2, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']]
      )
      
      #print('printing final before pushing to the updating function categorical output_meta from overview_projection_update_plot')
      #print(output_meta)
      #print('printing final before pushing to the updating function categorical output_data from overview_projection_update_plot')
      #print(output_data)
      #print('printing final before pushing to the updating function categorical output_hover from overview_projection_update_plot')
      #print(output_hover)
      #print('printing final before pushing to the updating function categorical output_group_centers from overview_projection_update_plot')
      #print(output_group_centers)
      
      
      #print('printing output_data for diags')
      #print(output_data)
      
      #output_data[['selectedpoints']] <- selected_cells
      #print('printing head of selected_cells')
      #print(head(selected_cells, 20))
      
      saveRDS(list(output_meta, output_data, output_hover, output_group_centers), file = 'output4updatePlot2DCategorical.RDS', compress = T)
      shinyjs::js$updatePlot2DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['z']][[i]] <- coordinates[[3]][cells_to_extract]
        output_data[['identifier']][[i]] <- rownames(coordinates)[cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            cells_df[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      output_data[['zrange']] <- plot_parameters[["z_range"]]
      group_centers_df <- centerOfGroups(coordinates, cells_df, 3, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']],
        z = group_centers_df[['z_median']]
      )
      
    
      
      
      #print('printing final before pushing to the updating function categorical output_meta from overview_projection_update_plot')
      #print(output_meta)
      #print('printing final before pushing to the updating function categorical output_data from overview_projection_update_plot')
      #print(output_data)
      #print('printing final before pushing to the updating function categorical output_hover from overview_projection_update_plot')
      #print(output_hover)
      #print('printing final before pushing to the updating function categorical output_group_centers from overview_projection_update_plot')
      #print(output_group_centers)
      
      shinyjs::js$updatePlot3DCategorical(
        output_meta,
        output_data,
        output_hover,
        output_group_centers
      )
    }
  }
}
