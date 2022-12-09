
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
        width = 10,
        collapsed = TRUE,
        plotlyOutput(ns("plot_volcano_plot")) %>% withSpinner(type = 8)
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


tab_expression_analysis_server <- function(id, tp) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observe({
      
      if (is.null(tp()) | {input$select_group_one == input$select_group_two}) shinyjs::disable("action_expression_analysis") else shinyjs::enable("action_expression_analysis")
      
    })
    
    
    experiments <- reactiveVal(NULL)
    

    observeEvent(tp(), {
      
      if (!setequal(experiments(), tp()$experiments$sample)) experiments(tp()$experiments$sample %>% unique())
      
    })
    
    
    observeEvent(experiments(), {

      updateSelectInput(session, "select_group_one",
        label = "Choose a sample group (ratio numerator)",
        choices = tp() %>%
          pluck("experiments") %>%
          pull(sample) %>%
          unique()
      )

      updateSelectInput(session, "select_group_two",
        label = "Choose a sample group (ratio denominator)",
        choices = tp() %>%
          pluck("experiments") %>%
          pull(sample) %>%
          unique()
      )
      
    })
    
    
    observeEvent(input$action_expression_analysis, {
      
      map(
        .x = c(
          "box_volcano_plot",
          "box_table_differential_expression"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
        
      tp(
        tp() %>% 
          tidyproteomics::expression(!!input$select_group_one/!!input$select_group_two,
            .method = statistical_methods[[input$select_statistical_method]]
          )
      )
      
    })
    
    
    output$plot_volcano_plot <- renderPlotly({
      
      input$action_expression_analysis
      
      isolate({
        tp()$analysis[[glue("{input$select_group_one}/{input$select_group_two}")]]$expression %>% 
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
        
        shiny::req(tp()$analysis)
      
        tp()$analysis[[glue("{input$select_group_one}/{input$select_group_two}")]]$expression %>% 
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