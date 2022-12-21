
tab_expression_analysis_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Expression analysis",
      tabName = "tab_expression_analysis",
      icon = icon("volcano"),
      condition = "output['tab_expression_analysis-tab_subset_availability'] == true"
    ),
    body = tabItem(
      tabName = "tab_expression_analysis",
      
      box(
        title = "Differential expression parameters",
        shinyjs::disabled(
          awesomeCheckbox(ns("checkbox_use_normalized_values"),
            label = "Use preprocessed abundances?",
            value = FALSE
          )
        ),
        selectInput(ns("select_group_one"),
          label = "Select a sample group (ratio numerator)",
          choices = NULL
        ),
        selectInput(ns("select_group_two"),
          label = "Select a sample group (ratio denominator)",
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
        status = "info",
        id = ns("box_volcano_plot"),
        width = 12,
        height = 650,
        collapsed = TRUE,
        plotlyOutput(ns("plot_volcano_plot"), height = 600) %>% withSpinner(type = 8),
        sidebar = boxSidebar(
          id = ns("box_volcano_plot_sidebar"),
          easyClose = FALSE,
          # background = "#2d537c",
          background = "#e9e9e9",
          # width = 32,
          fluidRow(
            column(6,
              selectInput(ns("select_volcano_x_axis"),
                label = "Select an x-axis variable",
                choices = c(
                  "log2(foldchange)" = "log2_foldchange",
                  "foldchange" = "foldchange"
                ),
                selected = "log2_foldchange"
              )
            ),
            column(6,
              selectInput(ns("select_volcano_y_axis"),
                label = "Select a y-axis variable",
                choices = c(
                  "p-value" = "p_value",
                  "adjusted p-value" = "adj_p_value",
                  "-log10(p-value)" = "-log10(p_value)",
                  "-log10(adjusted p-value)" = "-log10(adj_p_value)"
                ),
                selected = "-log10(adj_p_value)"
              )
            )
          ),
          fluidRow(
            column(6,
              selectInput(ns("select_volcano_color"),
                label = "Select a color variable",
                choices = c(
                  "fixed" = "fixed",
                  "log10(average expression)" = "log10(average_expression)",
                  "log10(proportional expression)" = "log10(proportional_expression)",
                  "imputed" = "imputed",
                  "p-value" = "p_value",
                  "adjusted p-value" = "adj_p_value",
                  "-log10(p-value)" = "-log10(p_value)",
                  "-log10(adjusted p-value)" = "-log10(adj_p_value)"
                ),
                selected = "fixed"
              )
            ),
            column(6,
              colorPickr(ns("pick_volcano_color"),
                label = "Choose a color",
                selected = "#1049B0"
              )
            )
          ),
          fluidRow(
            column(6,
              sliderInput(ns("slider_volcano_marker_alpha"),
                label = "Set marker opacity",
                value = 0.5,
                min = 0,
                max = 1
              )
            )
          ),
          fluidRow(
            column(6,
              selectInput(ns("select_volcano_size"),
                label = "Select a size variable",
                choices = c(
                  "fixed" = "fixed",
                  "log10(average expression)" = "log10(average_expression)",
                  "log10(proportional expression)" = "log10(proportional_expression)",
                  "imputed" = "imputed",
                  "p-value" = "p_value",
                  "adjusted p-value" = "adj_p_value",
                  "-log10(p-value)" = "-log10(p_value)",
                  "-log10(adjusted p-value)" = "-log10(adj_p_value)"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(12,
              actionButton(ns("action_replot_volcano"),
                label = "Update plot"
              )
            )
          )
        )
      ),
      
      box(
        title = "Differential expression table",
        status = "info",
        id = ns("box_table_differential_expression"),
        width = 12,
        collapsed = TRUE,
        actionButton(ns("table_download"), 
          label = "Download table",
          icon = icon("download")
        ) %>% 
          shiny::tagAppendAttributes(
            onclick = glue("Reactable.downloadDataCSV('{ns('table_differential_expression')}', 'expression_analysis.csv')")
          ),
        reactableOutput(ns("table_differential_expression")) %>% withSpinner(type = 8)
      )
      
    )
  )
  
}


tab_expression_analysis_server <- function(id, tp, tp_subset, tp_normalized, tp_expression) {
  
  moduleServer(id, function(input, output, session) {
    
    
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp())) FALSE else if (tp()$analyte == "peptides") FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    observe({
      
      if (is.null(tp()) | {input$select_group_one == input$select_group_two}) shinyjs::disable("action_expression_analysis") else shinyjs::enable("action_expression_analysis")
      
    })
    
    
    
    experiments <- reactiveVal(NULL)

    observeEvent(tp(), {
      
      if (!setequal(experiments(), tp()$experiments$sample)) experiments(tp()$experiments$sample %>% unique())
      
    })
    
    
    observe({
      
      if (input$select_volcano_color == "fixed") shinyjs::show("pick_volcano_color") else shinyjs::hide("pick_volcano_color")
      
    })
    

    observe({
      
      shiny::req(tp())
      
      # Use normalized object (only non-null through normalization only or subset+normalization)
      # for contrast construction
      if (!is.null(tp_normalized())) {

        if (!setequal(experiments(), tp_normalized()$experiments$sample)) experiments(tp_normalized()$experiments$sample %>% unique())
      
      # Use subset object (when normalization not active))
      # for contrast construction
      } else if (is.null(tp_normalized()) & !is.null(tp_subset())) {
        
        if (!setequal(experiments(), tp_subset()$experiments$sample)) experiments(tp_subset()$experiments$sample %>% unique())
        
      } else {
       
        if (!setequal(experiments(), tp()$experiments$sample)) experiments(tp()$experiments$sample %>% unique())
         
      }
        
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
          label = "Select a sample group (ratio numerator)",
          choices = experiments(),
          selected = experiments()[1]
        )
  
        updateSelectInput(session, "select_group_two",
          label = "Select a sample group (ratio denominator)",
          choices = experiments(),
          selected = experiments()[2]
        )
        
        shinyjs::enable("action_expression_analysis")
        
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
          choices = experiments(),
          selected = experiments()[1]
        )
        
        updateSelectInput(session, "select_group_two",
          label = "Differential expression analysis requires > 1 sample group",
          choices = experiments(),
          selected = experiments()[1]
        )
        
        shinyjs::disable("action_expression_analysis")
         
      }
      
    })
    
    
    observe({
      
      if (!is.null(tp_normalized()) & length(experiments()) > 1) {
      
        updateAwesomeCheckbox(session, "checkbox_use_normalized_values",
          label = "Use preprocessed abundances?",
          value = TRUE
        )
        
        shinyjs::enable("checkbox_use_normalized_values")
        
      } else {
        
        updateAwesomeCheckbox(session, "checkbox_use_normalized_values",
          label = "Use preprocessed abundances?",
          value = FALSE
        )
        
        shinyjs::disable("checkbox_use_normalized_values")
        
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
    
    
    tp_expression_analysis_annotated <- eventReactive(input$action_expression_analysis, {
      
      annotation_columns <- tp_expression()$annotation %>% 
        filter(term %in% c("gene_name", "description")) %>% 
        distinct(term) %>% 
        pull() %>% 
        sort(decreasing = TRUE)
      
      map(
        .x = tp_expression()$analysis, 
        .f = ~ .x$expression %>% 
          left_join(
            y = tp_expression()$annotation %>% 
              filter(term %in% c("gene_name", "description")) %>%
              tidyr::pivot_wider(id_cols = "protein", names_from = "term", values_from = "annotation")
          ) %>% 
          relocate(contains(annotation_columns), .after = "protein") %>% 
          {
            if ("description" %in% colnames(.)) {
              mutate(., description = {description %>% stringr::str_match(pattern = stringr::regex('^(.*) OS'))}[,2])
            } else .
          }
      )
      
      
      
    })
    
    
    output$plot_volcano_plot <- renderPlotly({
      
      input$action_expression_analysis
      input$action_replot_volcano
      
      isolate({
        
        # browser()
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression_analysis_annotated() %>%
          pluck(glue("{input$select_group_one}/{input$select_group_two}")) %>%
          plotly::plot_ly(
            type = "scattergl",
            x = ~ isolate(eval(parse_expr(input$select_volcano_x_axis))),
            y = ~ isolate(eval(parse_expr(input$select_volcano_y_axis))),
            color = if (input$select_volcano_color != "fixed") {~ isolate(eval(parse_expr(input$select_volcano_color)))},
            marker = if (input$select_volcano_color == "fixed") list(
              color =  input$pick_volcano_color,
              line = list(width = 0),
              opacity = input$slider_volcano_marker_alpha
            ) else list(line = list(width = 0)),
            size = if (input$select_volcano_size != "fixed") {~ isolate(eval(parse_expr(input$select_volcano_size)))},
            sizes = c(10,100),
            alpha = input$slider_volcano_marker_alpha,
            hoverinfo = 'text',
            text = glue::glue("accession: {.$protein}
            {if ('gene_name' %in% colnames(.)) invisible(glue('gene name: {.$gene_name}'))}
            {if ('description' %in% colnames(.)) invisible(glue('description: {.$description}'))}
            log10 average expression: {formatC(.$average_expression, format = 'e', digits = 2)}
            log2FC: {round(.$log2_foldchange, digits = 3)}
            adjusted p-value: {formatC(.$adj_p_value, format = 'e', digits = 2)}
            imputed: {round(.$imputed, digits = 3)}")
          ) %>%
          plotly::layout(
            title = list(
              text = glue::glue("{input$select_group_one} vs. {input$select_group_two}")
            ),
            xaxis = list(
              title = input$select_volcano_x_axis,
              zeroline = FALSE,
              showline = TRUE,
              ticks = "outside"
            ),
            yaxis = list(
              title = input$select_volcano_y_axis,
              zeroline = FALSE,
              showline = TRUE,
              ticks = "outside"
            ),
            font = list(
              family = "Segoe UI"
            ),
            margin = list(b = 80, l = 80, r = 80, t = 80)
          ) %>% 
          plotly::colorbar(
            title = if (input$select_volcano_color != "fixed") input$select_volcano_color else NULL
          )

      })
      
    })
    
    
    output$table_differential_expression <- reactable::renderReactable({
      
      input$action_expression_analysis
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression_analysis_annotated() %>% 
          pluck(glue("{input$select_group_one}/{input$select_group_two}")) %>%
          arrange(adj_p_value) %>% 
          reactable(
            sortable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(
              cell = render_expression_reactable
            ),
            columns = list(
              protein = colDef(
                cell = JS("function(cellInfo, state) {
                    return `<a href='https://www.uniprot.org/uniprotkb/${cellInfo.value}/entry' target='_blank' rel='noopener noreferrer'>${cellInfo.value}</a>`
                  }"
                ),
                html = TRUE
              ),
              description = colDef(
                minWidth = 300
              )
            )
          )
        
      })
      
    })
    
  })
  
}