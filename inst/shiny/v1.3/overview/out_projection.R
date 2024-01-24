##----------------------------------------------------------------------------##
## Plotly plot of the selected projection.
##----------------------------------------------------------------------------##

output[["overview_projection"]] <- plotly::renderPlotly({
  #print('printing input')
  #print(input)
  if (
    is.null(input[["overview_projection_to_display"]]) ||
    is.na(input[["overview_projection_to_display"]]) ||
    input[["overview_projection_to_display"]] %in% availableProjections() == FALSE
  ) {
    projection_to_display <- availableProjections()[1]
  } else {
    projection_to_display <- input[["overview_projection_to_display"]]
  }
   XYranges <- getXYranges(getProjection(projection_to_display))
  
  #warning('printing projection_to_display')
  #print(getProjection(projection_to_display))
  #warning('printing str of projection_to_display')
  #str(getProjection(projection_to_display))
  
  xrange <- XYranges$x
  yrange <- XYranges$y
  zrange <- XYranges$z
  
  
  #print('printing xrange')
  #print(xrange)
  #print('printing yrange')
  #print(yrange)
  #print('printing zrange, if applicable')
  #print(zrange)
  
  
  xrange_abs_0.2 <- (xrange$max-xrange$min)*0.5
  yrange_abs_0.2 <- (xrange$max-xrange$min)*0.5
  zrange_abs_0.2 <- (zrange$max-zrange$min)*0.5
  
  if(ncol(getProjection(projection_to_display))>2){
    
    print('drawing 3d scatter overview')
    
    plotly::plot_ly(type = 'scattergl', mode = 'markers', source = "overview_projection") %>%
      plotly::layout(scene = list(
        xaxis = list(
          #autorange = T,
          mirror = TRUE,
          showline = F,
          zeroline = FALSE,
          range=c(xrange$min-xrange_abs_0.2, xrange$max+xrange_abs_0.2)
        ),
        yaxis = list(
          #autorange = T,
          mirror = TRUE,
          showline = F,
          zeroline = FALSE,
          range=c(yrange$min-yrange_abs_0.2, yrange$max+yrange_abs_0.2)
        ),
        zaxis = list(
          #autorange = T,
          mirror = TRUE,
          showline = F,
          zeroline = FALSE,
          range=c(zrange$min-zrange_abs_0.2, zrange$max+zrange_abs_0.2)
        )
      ))
  } else {
    plotly::plot_ly(type = 'scattergl', mode = 'markers', source = "overview_projection") %>%
      plotly::layout(scene = list(
        xaxis = list(
          #autorange = T,
          mirror = TRUE,
          showline = F,
          zeroline = FALSE,
          autorange = F, range=c(xrange$min-xrange_abs_0.2, xrange$max+xrange_abs_0.2)
        ),
        yaxis = list(
          #autorange = T,
          mirror = TRUE,
          showline = F,
          zeroline = FALSE,
          autorange = F, range=c(yrange$min-yrange_abs_0.2, yrange$max+yrange_abs_0.2)
        )
      ))
  }
  
  
  
  
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_info"]], {
  showModal(
    modalDialog(
      overview_projection_info[["text"]],
      title = overview_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML("
    Interactive projection of cells into 2-dimensional space based on their expression profile.
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cells can be colored by the sample they came from, the cluster they were assigned, the number of transcripts or expressed genes, percentage of mitochondrial and ribosomal gene expression, an apoptotic score (calculated based on the expression of few marker genes; more info in the 'Sample info' tab on the left), or cell cycle status (determined using the Seurat and Cyclone method).</li>
      <li>Confidence ellipses show the 95% confidence regions.</li>
      <li>Samples and clusters can be removed from the plot individually to highlight a contrast of interest.</li>
      <li>By default, the point size is set to 15 without any transparency but both these attributes can be changed using the sliders on the left. The point size can also be set to reflect the number of transcripts or expressed genes.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs)</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
  )
)
