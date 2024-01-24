##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["overview_selected_cells_marker_genes_table_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Marker genes of selected cells")#,
        #cerebroInfoButton("overview_details_selected_cells_table_info")
      ),
      tagList(
        shinyWidgets::materialSwitch(
          inputId = "overview_selected_cells_marker_genes_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "overview_selected_cells_marker_genes_table_color_highlighting",
          label = "Highlight values with colors:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        DT::dataTableOutput("overview_selected_cells_marker_genes_table")
      )
    )
  )
})

output[["overview_selected_cells_marker_genes_table"]] <- DT::renderDataTable({
  req(
    overview_projection_selected_cells()
  )
  selected_cells <- overview_projection_selected_cells()
  #saveRDS(selected_cells, file = '~/Downloads/selected_cells.RDS', compress=T)
  
  #coords <- expression_projection_coordinates()
  #dat <- expression_projection_data()
  
  #saveRDS(coords, file = '~/Downloads/coords.RDS', compress = T)
  #saveRDS(dat, file = '~/Downloads/dat.RDS', compress = T)
  
  #cells_df <- bind_cols(
  #  coords,
  #  dat
  #)
  
  #cells_df$level <- expression_projection_expression_levels()
  
  #saveRDS(cells_df, file = '~/Downloads/cells_df.RDS', compress = T)
  
  #cells_df <- cells_df %>%
  #  dplyr::rename(X1 = 1, X2 = 2) %>%
  #  dplyr::mutate(identifier = paste0(X1, '-', X2)) %>%
  #  dplyr::filter(identifier %in% selected_cells$identifier) %>%
  #  dplyr::select(-c(X1, X2, identifier)) %>%
  #  dplyr::rename(expression_level = level) %>%
  #  dplyr::select(cell_barcode, expression_level, everything())
  
  #saveRDS(cells_df, file = '~/Downloads/cells_df_filtered.RDS', compress = T)
  
  expression_matrix <- getExpressionMatrix(
    cells = NULL, 
    genes = NULL, 
    dense = FALSE
  )
  
  selection_status <- rep('not_selected', ncol(expression_matrix))
  names(selection_status) <- colnames(expression_matrix)
  #selection_status[cells_df$cell_barcode] <- 'selected'
  #print('printing selected_cells$customdata')
  #print(selected_cells$customdata)
  selection_status[selected_cells$customdata] <- 'selected'
  
  #saveRDS(selection_status, '~/Downloads/selection_status.RDS', compress = T)
  prest <- presto::wilcoxauc(expression_matrix,
                             selection_status)
  #saveRDS(prest, '~/Downloads/prest.RDS', compress = T)
  
  output_table <- prest %>% 
    filter(padj < 0.05 ) %>% 
    filter(group=='selected') %>%
    dplyr::select(-5:-7) %>% 
    dplyr::select(-2) %>%
    dplyr::mutate(abs_logFC=abs(logFC)) %>% 
    arrange(desc(logFC))
  print('diag line 64 prime ok')
  
  #d1 <- DT::datatable(output_table,
  #                    extensions = 'Buttons', 
  #                    options = list(
  #                      dom = 'Bfrtip',
  #                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  d1 <- prettifyTable(
    output_table,
    filter = list(position = "top", clear = TRUE),
    dom = "Brtlip",
    show_buttons = TRUE,
    number_formatting = input[["overview_selected_cells_marker_genes_table_number_formatting"]],
    color_highlighting = input[["overview_selected_cells_marker_genes_table_color_highlighting"]],
    hide_long_columns = TRUE,
    download_file_name = "marker_genes_of_selected_cells_from_overview_pane",
    page_length_menu=c(15, 30, 50, 100, 1000)
  )
  print('diag line 71 ok')
  return(d1)
}#,
#server=FALSE, 
#rownames=FALSE
)
