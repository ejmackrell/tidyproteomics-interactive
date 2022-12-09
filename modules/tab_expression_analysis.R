
tab_expression_analysis_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Expression analysis",
      tabName = "tab_expression_analysis",
      icon = icon("volcano")
    ),
    body = tabItem(
      tabName = "tab_expression_analysis",
      
      box(
        title = "Differential expression parameters",
        shinyjs::disabled(
          awesomeCheckbox(ns("checkbox_use_normalized_values"),
            label = "Use normalized abundances?",
            value = FALSE
          )
        ),
        selectInput(ns("select_group_one"),
          label = "Choose a sample group (ratio numerator)",
          choices = NULL
        ),
        selectInput(ns("select_group_two"),
          label = "Choose a sample group (ratio denominator)",
          choices = NULL
        ),
        selectInput(ns("select_statistical_method"),
          label = "Select a statistical method",
          choices = c(
            "t-test",
            "Wilcoxon test",
            "Kolmogorov-Smirnov test",
            "limma"
          )
        ),
        actionButton(ns("action_expression_analysis"),
          label = "Run expression analysis"
        )
      ),
      
      box(
        title = "Volcano plot",
        id = ns("box_volcano_plot"),
        width = 12,
        height = 650,
        collapsed = TRUE,
        plotlyOutput(ns("plot_volcano_plot"), height = 600) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Differential expression table",
        id = ns("box_table_differential_expression"),
        width = 12,
        collapsed = TRUE,
        reactableOutput(ns("table_differential_expression")) %>% withSpinner(type = 8)
      )
      
    )
  )
  
}


tab_expression_analysis_server <- function(id, tp, tp_normalized, tp_expression) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observe({
      
      if (is.null(tp()) | {input$select_group_one == input$select_group_two}) shinyjs::disable("action_expression_analysis") else shinyjs::enable("action_expression_analysis")
      
    })
    
    
    experiments <- reactiveVal(NULL)
    

    observeEvent(tp(), {
      
      if (!setequal(experiments(), tp()$experiments$sample)) experiments(tp()$experiments$sample %>% unique())
      
    })
    
    
    observeEvent(experiments(), {
      
      if (length(experiments()) > 1) {
        
        map(
          .x = c(
            "select_group_one",
            "select_group_two"
          ),
          .f = ~ shinyjs::enable(.x)
        )

        updateSelectInput(session, "select_group_one",
          label = "Choose a sample group (ratio numerator)",
          choices = experiments(),
          selected = experiments()[1]
        )
  
        updateSelectInput(session, "select_group_two",
          label = "Choose a sample group (ratio denominator)",
          choices = experiments(),
          selected = experiments()[2]
        )
        
      } else {
        
        map(
          .x = c(
            "select_group_one",
            "select_group_two"
          ),
          .f = ~ shinyjs::disable(.x)
        )
        
        updateSelectInput(session, "select_group_one",
          label = "Differential expression analysis requires > 1 sample group",
          choices = NULL
        )
        
        updateSelectInput(session, "select_group_two",
          label = "Differential expression analysis requires > 1 sample group",
          choices = NULL
        )
         
      }
      
    })
    
    
    observe({
      
      if (is.null(tp_normalized()) & length(experiments()) > 1) {
      
        updateAwesomeCheckbox(session, "checkbox_use_normalized_values",
          label = "Use normalized abundances?",
          value = FALSE
        )
        
        shinyjs::disable("checkbox_use_normalized_values")
        
      } else {
        
        updateAwesomeCheckbox(session, "checkbox_use_normalized_values",
          label = "Use normalized abundances?",
          value = TRUE
        )
        
        shinyjs::enable("checkbox_use_normalized_values")
        
      }
      
    })
    
    
    observeEvent(input$action_expression_analysis, {
      
      map(
        .x = c(
          "box_volcano_plot",
          "box_table_differential_expression"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      if (input$checkbox_use_normalized_values) {
        
        tp_expression(
          tp_normalized() %>% 
            tidyproteomics::expression(!!input$select_group_one/!!input$select_group_two,
              .method = statistical_methods[[input$select_statistical_method]]
            )
        )
      
      } else {
        
        tp_expression(
          tp() %>% 
            tidyproteomics::expression(!!input$select_group_one/!!input$select_group_two,
              .method = statistical_methods[[input$select_statistical_method]]
            )
        )
        
      }
      
    })
    
    
    output$plot_volcano_plot <- renderPlotly({
      
      input$action_expression_analysis
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression()$analysis[[glue("{input$select_group_one}/{input$select_group_two}")]]$expression %>% 
          plotly::plot_ly(
            type = "scattergl",
            x = ~ log2_foldchange,
            y = ~ -log10(p_value),
            color = ~ imputed,
            size = ~ log10(average_expression),
            sizes = c(1,100),
            alpha = 0.5,
            hoverinfo = 'text',
            text = glue::glue("accession: {.$protein}
            log10 average expression: {formatC(.$average_expression, format = 'e', digits = 2)}
            log2FC: {round(.$log2_foldchange, digits = 3)}
            adjusted p-value: {formatC(.$adj_p_value, format = 'e', digits = 2)}
            imputed: {round(.$imputed, digits = 3)}")
          ) %>% 
            plotly::layout(
              title = list(
                text = glue::glue("{input$select_group_one} vs. {input$select_group_two}")
              ),
              margin = list(b = 80, l = 80, r = 80, t = 80)
            )
      })
      
    })
    
    
    output$table_differential_expression <- reactable::renderReactable({
      
      input$action_expression_analysis
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
      
        tp_expression()$analysis[[glue("{input$select_group_one}/{input$select_group_two}")]]$expression %>% 
          reactable(
            sortable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(
              cell = render_fixed_reactable
            ),
            columns = list(
              protein = colDef(
                cell = JS("function(cellInfo, state) {
                    return `<a href='https://www.uniprot.org/uniprotkb/${cellInfo.value}/entry' target='_blank' rel='noopener noreferrer'>${cellInfo.value}</a>`
                  }"
                ),
                html = TRUE
              )
            )
          )
        
      })
      
    })
    
  })
  
}