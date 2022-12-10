
tab_enrichment_analysis_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Enrichment analysis",
      tabName = "tab_enrichment_analysis",
      icon = icon("project-diagram")
    ),
    body = tabItem(
      tabName = "tab_enrichment_analysis",
      
      box(
        title = "Enrichment parameters",
        selectInput(ns("select_contrast"),
          label = "Select an existing comparison",
          choices = NULL
        ),
        selectInput(ns("select_ontology"),
          label = "Select ontology for annotation enrichment",
          choices = NULL
        ),
        actionButton(ns("action_enrichment"),
          label = "Run enrichment"
        )
      ),
      
      box(
        title = "Annotation enrichment plot",
        id = ns("box_annotation_enrichment_plot"),
        width = 10,
        collapsed = TRUE,
        plotlyOutput(ns("plotly_annotation_enrichment")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Annotation enrichment table",
        id = ns("box_annotation_enrichment_table"),
        width = 12,
        collapsed = TRUE,
        reactableOutput(ns("table_annotation_enrichment")) %>% withSpinner(type = 8)
      )
      
    )
  )
  
}

tab_enrichment_analysis_server <- function(id, tp, tp_expression, tp_enrichment) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      if (is.null(tp_expression()$analysis %>% names()) | is.null(input$select_ontology)) {
        
        shinyjs::disable("action_enrichment")
        
      } else {
        
        shinyjs::enable("action_enrichment")
        
      }
      
      
      if (is.null(tp_expression()$analysis %>% names())) {
        
        shinyjs::disable("select_contrast")
        
        updateSelectInput(session, "select_contrast",
          label = "Please run an expression analysis to use this feature.",
          choices = NULL
        )
         
      } else {
        
        shinyjs::enable("select_contrast")
         
      }
      
      if (input$select_ontology == '') {
        
        updateSelectInput(session, "select_ontology",
          choices = NULL,
          label = "Please upload a dataset to use this feature."
        )
        
        shinyjs::disable("select_ontology")
        
      } else {
        
        shinyjs::enable("select_ontology")
        
      }
      
      
    })
    
    observeEvent(tp(), {
      
      updateSelectInput(session, "select_ontology",
        label = "Select an ontology for annotation enrichment",
        choices =  tp()$annotations %>% 
          distinct(term) %>% 
          pull() %>% 
          purrr::discard(~ .x %in% c("description", "gene_id_entrez", "gene_id_ensemble", "gene_name"))
      )
      
    })
    
    
    observeEvent(tp_expression(), {
      
      updateSelectInput(session, "select_contrast",
        label = "Select an existing comparison",
        choices = tp_expression()$analysis %>% names()
      )
      
    })
    
    
    observeEvent(input$action_enrichment, {
      
      map(
        .x = c(
          "box_annotation_enrichment_plot",
          "box_annotation_enrichment_table"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      tp_enrichment(
        tp_expression() %>% 
          enrichment(!!input$select_contrast, .term = input$select_ontology)
      )
      
    })
    
    
    output$plotly_annotation_enrichment <- renderPlotly({
      
      input$action_enrichment
      
      isolate({
        
        shiny::req(tp_enrichment())
        
        tp_enrichment() %>% 
          plot_enrichment(!!input$select_contrast, .term = input$select_ontology) %>% 
          ggplotly()
        
      })
      
    })
    
    
    output$table_annotation_enrichment <- renderReactable({
      
      input$action_enrichment
      
      isolate({
        
        tp_enrichment() %>% 
          pluck("analysis", input$select_contrast, "enrichment", input$select_ontology) %>%
          reactable(
            elementId = "test_table",
            sortable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(
              cell = render_enrichment_reactable,
              sortNALast = TRUE
            ),
            columns = list(
              annotation = colDef(
                minWidth = 300
              )
            )
          )
        
      })
      
    })
    
  })
  
}