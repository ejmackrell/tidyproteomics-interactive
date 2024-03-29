
tab_enrichment_analysis_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Enrichment analysis",
      tabName = "tab_enrichment_analysis",
      icon = icon("project-diagram"),
      condition = "output['tab_enrichment_analysis-tab_subset_availability'] == true"
    ),
    body = tabItem(
      tabName = "tab_enrichment_analysis",
      
      box(
        title = "Enrichment parameters",
        selectInput(ns("select_contrast"),
          label = "Select an existing comparison",
          choices = NULL
        ),
        selectizeInput(ns("select_ontology"),
          label = "Select ontology for annotation enrichment",
          choices = NULL
        ),
        selectInput(ns("select_method"),
          label = "Select an enrichment method",
          choices = c("gsea", "wilcoxon")
        ),
        br(),
        actionButton(ns("action_enrichment"),
          label = "Run enrichment"
        )
      ),
      
      box(
        title = "Annotation enrichment plot",
        status = "info",
        id = ns("box_annotation_enrichment_plot"),
        width = 12,
        collapsed = TRUE,
        plotlyOutput(ns("plotly_annotation_enrichment")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Annotation enrichment table",
        status = "info",
        id = ns("box_annotation_enrichment_table"),
        width = 12,
        collapsed = TRUE,
        actionButton(ns("table_download"), 
          label = "Download table",
          icon = icon("download")
        ),
        reactableOutput(ns("table_annotation_enrichment")) %>% withSpinner(type = 8)
      )
      
    )
  )
  
}


tab_enrichment_analysis_server <- function(id, tp, tp_expression, tp_enrichment) {
  
  moduleServer(id, function(input, output, session) {
    
    # Callback for downloading enrichment table
    shinyjs::onclick("table_download", runjs(glue("Reactable.downloadDataCSV('tab_enrichment_analysis-table_annotation_enrichment', '{stringr::str_split(input$select_contrast, '/')[[1]][1]}_vs_{stringr::str_split(input$select_contrast, '/')[[1]][2]}_enrichment_analysis.csv')")))
    
    
    # Hide enrichment tab when expression data are not available
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp_expression())) FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    # Disable enrichment if no expression analysis available
    observe({
      
      if (is.null(tp_expression()$analysis %>% names()) | input$select_ontology == '') {
        
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
      
    })
    
    
    # Disable enrichment if no ontology information available
    observeEvent(tp(), {
      
      if (!is.null(tp()$annotations)) {
      
        updateSelectInput(session, "select_ontology",
          label = "Select an ontology for annotation enrichment",
          choices =  tp()$annotations %>% 
            distinct(term) %>% 
            pull() %>% 
            purrr::discard(~ .x %in% c("description", "gene_id_entrez", "gene_id_ensemble", "gene_name"))
        )
        
        shinyjs::enable("select_ontology")
        
      } else {
        
        updateSelectizeInput(session, "select_ontology",
          label = "No annotations were provided in the uploaded data",
          choices = ''
        )
        
        shinyjs::disable("select_ontology")
        shinyjs::disable("action_enrichment")
        
      }
      
    })
    
    
    # Update contrast selection choice
    observeEvent(tp_expression(), {
      
      updateSelectInput(session, "select_contrast",
        label = "Select an existing comparison",
        choices = tp_expression()$analysis %>% names()
      )
      
    })
    
    
    observeEvent(input$action_enrichment, {
      
      # Toggle enrichment output boxes upon input
      map(
        .x = c(
          "box_annotation_enrichment_plot",
          "box_annotation_enrichment_table"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    # Set enrichment object with current expression object
    set_tp_enrichment <- eventReactive(input$action_enrichment, {
      
      tp_enrichment(
        tp_expression() %>% 
          enrichment(!!input$select_contrast, 
            .term = input$select_ontology,
            .method = input$select_method
          )
      )
      
      tp_enrichment()
      
    })
    
    
    # Render annotation enrichment plot
    output$plotly_annotation_enrichment <- renderPlotly({
      
      shiny::req(set_tp_enrichment())
      
      isolate({
        
        shiny::req(tp_enrichment())
        
        tp_enrichment() %>% 
          plot_enrichment(!!input$select_contrast, .term = input$select_ontology) %>% 
          ggplotly() %>% 
          layout(
            title = list(
              text = glue("{stringr::str_split(input$select_contrast, '/')[[1]][1]} vs. {stringr::str_split(input$select_contrast, '/')[[1]][2]}")
            ),
            font = list(
              size = 12
            )
          )
        
      })
      
    })
    
    
    # Render annotation enrichment table
    output$table_annotation_enrichment <- renderReactable({
      
      shiny::req(set_tp_enrichment())
      
      isolate({
        
        tp_enrichment_annotations <- tp_enrichment() %>% 
          export_analysis(!!input$select_contrast,
          .analysis = "expression"
          ) %>% 
          select(input$select_ontology, protein, {if ("gene_name" %in% colnames(.)) "gene_name"}, {if ("description" %in% colnames(.)) "description"}, log2_foldchange, adj_p_value) %>% 
          tidyr::separate_rows(input$select_ontology, sep = ";") %>% 
          distinct() %>% 
          if ("description" %in% colnames(.)) {
            mutate(., description = {description %>% stringr::str_match(pattern = stringr::regex('^(.*) OS'))}[,2])
          } else .

        tp_enrichment() %>%
          pluck("analysis", input$select_contrast, "enrichment", input$select_ontology) %>%
          reactable(
            sortable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(
              cell = render_enrichment_reactable,
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
            ),
            columns = list(
              annotation = colDef(
                minWidth = 300
              )
            ),
            details = function(index) {
              
              if (nrow(.) < 200) {
                
                htmltools::div(
                  class = "enrichment-subtable",
                  br(),
                  reactable(
                    data = filter(tp_enrichment_annotations, tp_enrichment_annotations[[input$select_ontology]] == .$annotation[index]) %>% 
                      arrange(adj_p_value),
                    compact = TRUE,
                    highlight = TRUE,
                    resizable = TRUE,
                    defaultColDef = colDef(
                      cell = render_expression_reactable,
                      headerStyle = list(background = "#FAFAFA")
                    )
                  ),
                  br()
                )
                
              } 
              
            }
          )
        
      })
      
    })
    
  })
  
}