
tab_upload_data_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Data input and summary",
      tabName = "tab_upload_data",
      icon = icon("file-arrow-up")
    ),
    body = tabItem(
      tabName = "tab_upload_data",
      
      fluidRow(
        box(
          title = "Table upload",
          # status = "secondary",
          selectInput(ns("select_data_type"),
            label = "Select a data type",
            choices = list(
              "Proteome Discoverer" = "ProteomeDiscoverer"#,
              # "MaxQuant" = "MaxQuant"
            ),
            width = "300px"
          ),
          selectInput(ns("select_analyte_type"),
            label = "Select an analyte type",
            choices = list(
              "Proteins" = "proteins"#,
              # "Peptides" = "peptides"
            ),
            selected = "proteins",
            width = "300px"
          ),
          br(),
          
          # Expand to allow .csv?
          fileInput(ns("upload_table"),
            label = "Upload a search file (.xlsx)",
            accept = c(".xlsx")
          ),
          
          br(),
          actionButton(ns("action_upload_table"),
            label = "Import data"
          )
        )
      ),
      
      box(
        title = "tidyproteomics object summary",
        status = "info",
        collapsed = TRUE,
        width = 10,
        id = ns("box_tidyproteomics_object_summary"),
        htmlOutput(ns("text_tidyproteomics_summary")) %>% withSpinner(type = 8, proxy.height = "250px")
      ),
      
      box(
        title = "Data feature selection",
        # status = "secondary",
        collapsed = TRUE,
        id = ns("box_data_feature_selection"),
        selectInput(ns("select_data_feature"),
          label = "Please upload a data table to use this feature",
          choices = NULL
        ),
        actionButton(ns("action_view_feature"),
          label = "View feature"
        )
      ),
      
      box(
        title = "Data feature viewer",
        status = "info",
        collapsed = TRUE,
        width = 12,
        id = ns("box_data_feature_viewer"),
        reactableOutput(ns("table_data_feature")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Summary table selection",
        # status = "secondary",
        collapsed = TRUE,
        id = ns("box_summary_table_selection"),
        selectInput(ns("select_summary_table"),
          label = "Please upload a data table to use this feature",
          choices = NULL
        ),
        actionButton(ns("action_summarize"),
          label = "Summarize data"
        )
      ),
        
      box(
        title = "Summary statistics",
        status = "info",
        id = ns("box_summary_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_sample")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Contaminant selection",
        # status = "secondary",
        id = ns("box_contaminant_selection"),
        collapsed = TRUE,
        textInput(ns("text_contaminant_pattern"),
          label = "Provide a contaminant pattern",
          placeholder = "e.g., CRAP"
        ),
        actionButton(ns("action_contaminant_summarize"),
          label = "Evaluate contaminant pattern"
        )
      ),
      
      box(
        title = "Contamination statistics",
        status = "info",
        id = ns("box_contamination_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_contamination")) %>% withSpinner(type = 8)
      )
      
    )
  )
}


tab_upload_data_server <- function(id, tp, tp_subset, tp_normalized) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      if (is.null(input$upload_table)) shinyjs::disable("action_upload_table") else shinyjs::enable("action_upload_table")
      
    })
    
    
    
    observeEvent(input$action_upload_table, {
      
      map(
        .x = c(
          "box_tidyproteomics_object_summary"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    set_tp <- eventReactive(input$action_upload_table, {
      
      tp(
        tidyproteomics::import(
          files = input$upload_table$datapath,
          platform = input$select_data_type,
          analyte = input$select_analyte_type
        )
      )
      
      # Reset subsetted and normalized objects upon upload of new data
      tp_subset(NULL)
      tp_normalized(NULL)
      
      tp()
      
    })
    
    
    output$text_tidyproteomics_summary <- renderUI({
      
      shiny::req(set_tp())

      isolate({
        
        map(
          .x = c(
            # "box_tidyproteomics_object_summary"
            "box_summary_table_selection",
            "box_contaminant_selection",
            "box_data_feature_selection"
          ),
          .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
        )
        
        HTML(paste0("<pre style='padding: 0rem; margin-bottom: 0rem; overflow: hidden;'>", capture.output(tp()), "</pre>"))
        
      })

    })
    
    
    observeEvent(input$action_summarize, {
      
      map(
        .x = c(
          "box_summary_statistics"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    observeEvent(input$action_contaminant_summarize, {
      
      map(
        .x = c(
          "box_contamination_statistics"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    observeEvent(input$action_view_feature, {
      
      map(
        .x = c(
          "box_data_feature_viewer"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    
    
    
    observeEvent(tp(), {
      
      updateSelectInput(session, "select_summary_table",
        label = "Select a summary variable",
        choices = tidyproteomics:::get_variables(tp())
      )
      
      updateSelectInput(session, "select_data_feature",
        label = "Select a data feature",
        choices = tabular_data_features[tabular_data_features %in% names(tp())]
      )
      
    })
    
    
    output$table_data_feature <- reactable::renderReactable({
      
      input$action_view_feature
      
      isolate({
        
        shiny::req(tp())
        
        tp() %>% 
          pluck(input$select_data_feature) %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            defaultColDef = colDef(
              sortNALast = TRUE
            )
          )
        
      })
      
    })
    
    
    output$table_summary_sample <- reactable::renderReactable({
      
      input$action_summarize
      
      isolate({
      
        shiny::req(tp())
        
        tp() %>% 
          summary(input$select_summary_table, destination = "return") %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            columns = list(
              CVs = colDef(
                cell = JS("function(cellInfo, state) {
                    if (cellInfo.value != null) {
                      return cellInfo.value.toFixed(3)
                    } else {
                      return cellInfo.value
                    }
                  }"
                )
              )
            )
          )
        
      })
          
      
    })
    
    
    output$table_summary_contamination <- reactable::renderReactable({
      
      input$action_contaminant_summarize
      
      isolate({
        
        shiny::req(tp())
        
        tp() %>% 
          summary(
            contamination = input$text_contaminant_pattern,
            destination = "return"
          ) %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE
          )
      })
      
    })
    
  })
}