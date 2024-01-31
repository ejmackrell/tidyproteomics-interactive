
tab_normalize_abundances_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Abundance normalization",
      tabName = "tab_normalize_abundances",
      icon = icon("scale-balanced"),
      condition = "output['tab_normalize_abundances-tab_subset_availability'] == true"
    ),
    body = tabItem(
      tabName = "tab_normalize_abundances",
      
      box(
        title = "Normalization selection",
        selectInput(ns("select_normalization_method"),
          label = "Select normalization methods for evaluation",
          width = "300px",
          choices = list(
            "median",
            "scaled",
            "linear",
            "limma",
            "loess",
            "svm",
            "randomforest"
          ),
          multiple = TRUE
        ),
        awesomeCheckbox(ns("checkbox_auto_normalization"),
          label = "Automatically select optimal normalization method?",
          value = TRUE
        ),
        hidden(
          selectInput(ns("select_auto_normalization"),
            label = "Select the user-specified normalization method",
            choices = NULL
          )
        ),
        br(),
        awesomeCheckbox(ns("checkbox_impute"),
          label = "Impute missing values?",
          value = TRUE
        ),
        selectInput(ns("select_imputation_order"),
          label = "Impute before or after normalization?",
          choices = c(
            "before",
            "after"
          ),
          selected = "after"
        ),
        selectInput(ns("select_impute_function"),
          label = "Select a function for imputation",
          choices = c(
            "min",
            "median",
            "randomforest"
          )
        ),
        selectInput(ns("select_impute_method"),
          label = "Select a method for imputation",
          choices = c(
            "row",
            "column",
            "matrix"
          )
        ),
        br(),
        actionButton(ns("action_normalize"),
          label = "Run normalization"
        )
      ),
      
      box(
        title = "Abundance boxplots",
        status = "info",
        id = ns("box_normalization_boxplots"),
        plotlyOutput(ns("plotly_normalization_boxplots")) %>% withSpinner(type = 8),
        width = 12,
        collapsed = TRUE
      ),
      
      box(
        title = "Abundance CVs and dynamic range",
        status = "info",
        id = ns("box_abundance_cvs_and_dynamic_range"),
        plotlyOutput(ns("plotly_abundance_cvs_and_dynamic_range")) %>% withSpinner(type = 8),
        plotOutput(ns("plot_dynamic_range"), height = "650px") %>% withSpinner(type = 8),
        width = 12,
        collapsed = TRUE
      ),
      
      box(
        title = "Principal component analysis (PCA)",
        status = "info",
        id = ns("box_pca"),
        width = 12,
        collapsed = TRUE,
        fluidRow(
          column(6,
            plotlyOutput(ns("plotly_pca")) %>% withSpinner(type = 8)
          ),
          column(6,
            plotlyOutput(ns("plotly_pca_variation")) %>% withSpinner(type = 8)
          )
        )
      ),
      
      box(
        title = "Clustered heatmap",
        status = "info",
        id = ns("box_heatmap"),
        width = 12,
        collapsed = TRUE,
        plotOutput(ns("plot_heatmap"), height = "650px") %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Abundance export",
        status = "info",
        id = ns("box_export"),
        width = 12,
        collapsed = TRUE,
        br(),
        actionButton(ns("table_download"), 
          label = "Download table",
          icon = icon("download")
        ),
        reactableOutput(ns("table_export")) %>% withSpinner(type = 8)
      )
      
    ),
    footer = tagList(
      htmlOutput(ns("normalization_output"))
    )
  )
  
}


tab_normalize_abundances_server <- function(id, tp, tp_subset, tp_normalized, tp_expression) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(input$checkbox_auto_normalization, {
      
      if (input$checkbox_auto_normalization) shinyjs::hide("select_auto_normalization") else shinyjs::show("select_auto_normalization")
      
    })
    
    
    observeEvent(input$select_normalization_method, {
      
      updateSelectInput(session, "select_auto_normalization", 
        choices = if (is.null(input$select_normalization_method)) "raw" else c("raw", input$select_normalization_method),
        selected = if (input$select_auto_normalization %in% input$select_normalization_method) input$select_auto_normalization else "raw"
      )
      
    }, ignoreNULL = FALSE)
    
    
    shinyjs::onclick("table_download", runjs(glue("Reactable.downloadDataCSV('tab_normalize_abundances-table_export', '{tp_normalized()$quantitative_source}_normalized_abundances_export.csv')")))
    
    
    # Hide tab when no data is available
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp())) FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    observeEvent(tp(), {
      
      # Hide clustered heatmap box if peptide data are used
      map(
        .x = c(
          "box_heatmap"
        ),
        .f = ~ {if (tp()$analyte == "peptides") shinyjs::hide(.x) else shinyjs::show(.x)}
      )
      
    })
    
    
    observe({
      
      # Disable normalization/imputation method selection if null data or inappropriate methods chosen
      if (is.null(tp()) | 
          is.null(input$select_normalization_method) |
          {input$checkbox_auto_normalization & is_empty(input$select_auto_normalization)}) shinyjs::disable("action_normalize") else shinyjs::enable("action_normalize")
      
      if (input$checkbox_impute == FALSE) {
        
        shinyjs::disable("select_imputation_order")
        shinyjs::disable("select_impute_function") 
        shinyjs::disable("select_impute_method")
        
      } else {
        
        shinyjs::enable("select_imputation_order")
        shinyjs::enable("select_impute_function")
        shinyjs::enable("select_impute_method")
        
      }
      
      
      if (input$select_impute_function == "randomforest") {
        
        shinyjs::hide("select_impute_method")
        
      } else {
        
        shinyjs::show("select_impute_method")
        
      }
      
    })
    
    
    observeEvent(input$action_normalize, {
      
      # Expand boxes containing plot outputs upon normalization
      map(
        .x = c(
          "box_normalization_boxplots",
          "box_abundance_cvs_and_dynamic_range",
          "box_pca",
          "box_heatmap",
          "box_export"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    set_tp_normalized <- eventReactive(input$action_normalize, {
      
      # Set normalized tp object given current tp object
      tp_normalized(
        
        {if (!is.null(tp_subset())) tp_subset() else tp()} %>% 
          {
            
            if (input$select_imputation_order == "before") {
              
              {
                if (input$checkbox_impute) {
                  
                  impute(.,
                    .function = imputation_methods[[input$select_impute_function]],
                    method = if (input$select_impute_function == "randomforest") "matrix" else input$select_impute_method
                  )
                  
                } else .
                
              } %>%
              normalize(
                .method = input$select_normalization_method
              ) %>% {
                
                if (!input$checkbox_auto_normalization) {
                  
                  select_normalization(., normalization = input$select_auto_normalization)
                  
                } else .
                
              }
                
              
            } else if (input$select_imputation_order == "after") {
            
              normalize(.,
                .method = input$select_normalization_method
              ) %>%  
              {
                
                if (input$checkbox_impute) {
                  
                  impute(.,
                    .function = imputation_methods[[input$select_impute_function]],
                    method = if (input$select_impute_function == "randomforest") "matrix" else input$select_impute_method
                  )
                  
                } else .
                
              } %>% {
                
                if (!input$checkbox_auto_normalization) {
                  
                  select_normalization(., normalization = input$select_auto_normalization)
                  
                } else .
                
              } 
            
            }
            
          }
      )
      
      # Clear downstream object
      tp_expression(NULL)
      
      tp_normalized()
      
    })
    
    
    # Render boxplots
    output$plotly_normalization_boxplots <- renderPlotly({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        tp_normalized() %>% 
          plot_normalization() %>% 
          ggplotly() %>% 
          layout(
            boxmode = "group",
            font = list(
              family = "Segoe UI"
            )
          )
        
      })
        
    })
    
    
    # Render CVs vs. method plots
    output$plotly_abundance_cvs_and_dynamic_range <- renderPlotly({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        tp_normalized() %>% 
          plot_variation_cv_mod() %>% 
          ggplotly() %>% 
          plotly::layout(
            font = list(
              family = "Segoe UI"
            )
          )
        
      })
      
    })
    
    
    # Render CVs + dynamic range hexbin and regression plots
    output$plot_dynamic_range <- renderPlot({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        tp_normalized() %>% 
          plot_dynamic_range()
        
      })
      
    })
    
    
    # Render interactive PCA scores plot
    output$plotly_pca <- renderPlotly({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        pca_ggplot <- tp_normalized() %>% 
          plot_pca_mod()
          
        pca_ggplot %>% 
          ggplotly() %>% 
          plotly::layout(
            margin = list(t = 100),
            title = list(text = glue("{pca_ggplot$labels$title}<br><sup>{pca_ggplot$labels$subtitle}</sup>")),
            font = list(
              family = "Segoe UI"
            )
          )
        
      })
      
    })
    
    
    # Render cumulative variance explained plot
    output$plotly_pca_variation <- renderPlotly({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        tp_normalized() %>% 
          plot_variation_pca() %>% 
          ggplotly() %>% 
          plotly::layout(
            margin = list(t = 100),
            font = list(
              family = "Segoe UI"
            )
          )
        
      })
      
    })
    
    
    # Render clustered heatmap
    output$plot_heatmap <- renderPlot({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        if (tp_normalized()$analyte == "peptides") {
          
          NULL
          
        } else {
        
        tp_normalized() %>% 
          plot_heatmap()
          
        }
        
      })
      
    })
    
    # Render abundances export
    output$table_export <- renderReactable({
      
      shiny::req(set_tp_normalized())
      
      isolate({
        
        tp_normalized() %>% 
          as.data.frame(shape = "wide") %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            wrap = FALSE,
            defaultColDef = colDef(
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
            )
          )
        
      })
      
    })
    
    
    # Render normalization indicator in footer
    output$normalization_output <- renderUI({
      
      tp()
      tp_normalized()
      
      isolate({
      
        if (!is.null(tp())) {
        
          if (!is.null(tp_normalized()$quantitative_source)) {
            
            if (input$checkbox_impute) {
            
              display_normalization_html <- glue('<span style="font-weight:300;font-size:0.75rem;">impute {input$select_imputation_order} normalization</span>')
              HTML(glue('<span class="footer-information">normalized: {tp_normalized()$quantitative_source}<br>{display_normalization_html}</span>'))
              
            } else {
              
              display_normalization_html <- glue('<span style="font-weight:300;font-size:0.75rem;">no imputation</span>')
              HTML(glue('<span class="footer-information">normalized: {tp_normalized()$quantitative_source}<br>{display_normalization_html}</span>'))
              
            }
            
          } else {
            
            HTML(glue('<span class="footer-information-warning">normalized: false</span>'))
            
          }
          
        }
        
      })
      
    })
    
    
      
    
  })
  
}