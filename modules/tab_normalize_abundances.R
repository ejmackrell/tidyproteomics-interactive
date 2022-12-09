
tab_normalize_abundances_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Normalize abundances",
      tabName = "tab_normalize_abundances",
      icon = icon("scale-balanced")
    ),
    body = tabItem(
      tabName = "tab_normalize_abundances",
      
      box(
        title = "Normalization selection",
        awesomeCheckbox(ns("checkbox_drop_contaminants"),
          label = "Drop contaminant proteins?",
          value = TRUE
        ),
        awesomeCheckbox(ns("checkbox_impute"),
          label = "Impute missing values?",
          value = TRUE
        ),
        selectInput(ns("select_impute_function"),
          label = "Select a function for imputation",
          choices = c(
            "min",
            "median",
            "mean",
            "max",
            "sum"
          )
        ),
        selectInput(ns("select_normalization_method"),
          label = "Select normalization methods",
          choices = list(
            "median",
            "linear",
            "limma",
            "loess",
            "randomforest"
          ),
          multiple = TRUE
        ),
        actionButton(ns("action_normalize"),
          label = "Run normalization"
        )
      ),
      
      box(
        title = "Abundance boxplots",
        id = ns("box_normalization_boxplots"),
        plotlyOutput(ns("plotly_normalization_boxplots")) %>% withSpinner(type = 8),
        width = 12,
        collapsed = TRUE
      ),
      
      box(
        title = "Abundance CVs and dynamic range",
        id = ns("box_abundance_cvs_and_dynamic_range"),
        plotlyOutput(ns("plotly_abundance_cvs_and_dynamic_range")) %>% withSpinner(type = 8),
        plotOutput(ns("plot_dynamic_range")) %>% withSpinner(type = 8),
        width = 12,
        collapsed = TRUE
      ),
      
      box(
        title = "Principal component analysis (PCA)",
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
        id = ns("box_heatmap"),
        width = 12,
        collapsed = TRUE,
        plotOutput(ns("plot_heatmap")) %>% withSpinner(type = 8)
        # InteractiveComplexHeatmapOutput(ns("plot_interactive_heatmap"))
      )
      
    )
  )
  
}

tab_normalize_abundances_server <- function(id, tp) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      if (is.null(tp()) | is.null(input$select_normalization_method)) shinyjs::disable("action_normalize") else shinyjs::enable("action_normalize")
      
    })
    
    observeEvent(input$action_normalize, {
      
      map(
        .x = c(
          "box_normalization_boxplots",
          "box_abundance_cvs_and_dynamic_range",
          "box_pca",
          "box_heatmap"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      # Update tidyproteomics object with normalization
      tp(
        tp() %>% 
          {
            if(input$checkbox_drop_contaminants) {
              subset(., description %!like% "^CRAP")
            }
          } %>% 
          {
            if (input$checkbox_impute) {
              impute(., impute_function = stats::median)
            }
          } %>% 
          normalize(
            .method = input$select_normalization_method
          )
      )
      
    })
    
    
    output$plotly_normalization_boxplots <- renderPlotly({
      
      input$action_normalize
      
      isolate({
        
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_normalization() %>% 
          ggplotly() %>% 
          layout(boxmode = "group")
        
      })
        
    })
    
    
    
    
    output$plotly_abundance_cvs_and_dynamic_range <- renderPlotly({
      
      input$action_normalize
      
      isolate({
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_variation_cv() %>% 
          ggplotly()
        
      })
      
    })
    
    
    output$plot_dynamic_range <- renderPlot({
      
      input$action_normalize
      
      isolate({
        
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_dynamic_range()
        
      })
      
    })
    
    
    output$plotly_pca <- renderPlotly({
      
      input$action_normalize
      
      isolate({
        
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_pca() %>% 
          ggplotly()
        
      })
      
    })
    
    output$plotly_pca_variation <- renderPlotly({
      
      input$action_normalize
      
      isolate({
        
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_variation_pca() %>% 
          ggplotly()
        
      })
      
    })
    
    output$plot_heatmap <- renderPlot({
      
      input$action_normalize
      
      isolate({
        
        shiny::req(tp())
        shiny::req(input$action_normalize)
        
        tp() %>% 
          plot_heatmap()
        
      })
      
    })
    
    
      
    
  })
  
}