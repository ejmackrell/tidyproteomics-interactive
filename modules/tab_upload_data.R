
tab_upload_data_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Upload data",
      tabName = "tab_upload_data",
      icon = icon("file-arrow-up")
    ),
    body = tabItem(
      tabName = "tab_upload_data",
      
      fluidRow(
        box(
          title = "Table upload",
          selectInput(ns("select_data_type"),
            label = "Select a data type",
            choices = list(
              "Proteome Discoverer" = "ProteomeDiscoverer",
              "MaxQuant" = "MaxQuant"
            ),
            selected = "mq",
            width = "300px"
          ),
          selectInput(ns("select_analyte_type"),
            label = "Select an analyte type",
            choices = list(
              "Proteins" = "proteins",
              "Peptides" = "peptides"
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
        ),
        box(
          title = "tidyproteomics object summary",
          collapsed = TRUE,
          id = ns("box_tidyproteomics_object_summary"),
          htmlOutput(ns("text_tidyproteomics_summary")) %>% withSpinner(type = 8, proxy.height = "250px")
        )
      ),
      
      box(
        title = "Summary statistics",
        id = ns("box_summary_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_sample")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Contamination statistics",
        id = ns("box_contamination_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_contamination")) %>% withSpinner(type = 8)
      )
      
    )
  )
}


tab_upload_data_server <- function(id, tp) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observe({
      
      if (is.null(input$upload_table)) shinyjs::disable("action_upload_table") else shinyjs::enable("action_upload_table")
      
    })
    
    
    observeEvent(input$action_upload_table, {
      
      map(
        .x = c(
          "box_summary_statistics",
          "box_contamination_statistics",
          "box_tidyproteomics_object_summary"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      # Import data as tidyproteomics object
      imported_tp <- tidyproteomics::import(
        files = input$upload_table$datapath,
        platform = input$select_data_type,
        analyte = input$select_analyte_type
      )
      
      # Add some validate/need statements for mismatched data
      tp(imported_tp)
      
    })
    
    output$text_tidyproteomics_summary <- renderUI({
      
      input$action_upload_table
      
      isolate({
        HTML(paste0("<pre style='padding: 0rem; margin-bottom: 0rem; overflow: hidden;'>",capture.output(tp()), "</pre>"))
      })
      
    })
    
    
    output$table_summary_sample <- reactable::renderReactable({
      
      input$action_upload_table
      
      isolate({
      
        shiny::req(tp())
        
        tp() %>% 
          summary("sample",
            destination = "return"
          ) %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            columns = list(
              CVs = colDef(
                cell = JS("function(cellInfo, state) {
                    return cellInfo.value.toFixed(3)
                  }"
                )
              )
            )
          )
        
      })
          
      
    })
    
    
    output$table_summary_contamination <- reactable::renderReactable({
      
      input$action_upload_table
      
      isolate({
        shiny::req(tp())
        
        tp() %>% 
          summary(
            contamination = "CRAP",
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