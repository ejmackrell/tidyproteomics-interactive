
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
        # shinyjs::disabled(
        #   awesomeCheckbox(ns("checkbox_use_normalized_values"),
        #     label = "Use preprocessed abundances?",
        #     value = FALSE
        #   )
        # ),
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
          ),
          width = "200px"
        ),
        br(),
        actionButton(ns("action_expression_analysis"),
          label = "Run expression analysis"
        )
      ),
      
      tabBox(
        status = "info",
        id = ns("box_plots"),
        type = "tabs",
        width = 12,
        height = 650,
        collapsed = TRUE,
        tabPanel(
          title = "Interactive plot",
          plotlyOutput(ns("plot_interactive_plot"), height = 600) %>% withSpinner(type = 8),
          value = ns("box_interactive_volcano")
        ),
        tabPanel(
          title = "Static volcano plot",
          plotOutput(ns("plot_static_volcano_plot"), height = 600) %>% withSpinner(type = 8),
          value = ns("box_static_volcano")
        ),
        tabPanel(
          title = "Static proportion plot",
          plotOutput(ns("plot_static_proportion_plot"), height = 600) %>% withSpinner(type = 8),
          value = ns("box_static_proportion")
        ),
        sidebar = boxSidebar(
          id = ns("box_volcano_plot_sidebar"),
          easyClose = FALSE,
          background = "#e9e9e9",
          div(
            id = ns("options_static_volcano"),
            fluidRow(
              column(6,
                checkboxInput(ns("checkbox_static_volcano_panels"), "Show colored fold change panels?", value = TRUE),
                checkboxInput(ns("checkbox_static_volcano_lines"), "Show fold change thresholds?", value = TRUE),
                checkboxInput(ns("checkbox_static_volcano_fc_scale"), "Show ratio scale?", value = TRUE),
                selectInput(ns("select_static_volcano_sig_column"), "Sigificance column", 
                  choices = c("p-value" = "p_value", "adjusted p-value" = "adj_p_value"),
                  selected = "adj_p_value"
                )
              ),
              column(6,
                sliderInput(ns("slider_static_volcano_sig_max"), "Significance threshold", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                sliderInput(ns("slider_static_volcano_fc_min"), "Fold change threshold", min = 0.5, max = 4, value = 1, step = 0.1),
                colorPickr(ns("pick_static_volcano_positive"), "Upregulated protein color", selected = "#1e90ff"),
                colorPickr(ns("pick_static_volcano_negative"), "Downregulated protein color", selected = "#ff3030")
              )
            ),
            br(),
            fluidRow(
              column(12,
                actionButton(ns("action_replot_static_volcano"),
                  label = "Update plot"
                )
              )
            )
          ),
          div(
            id = ns("options_static_proportion"),
            fluidRow(
              column(6,
                checkboxInput(ns("checkbox_static_proportion_panels"), "Show colored fold change panels?", value = TRUE),
                checkboxInput(ns("checkbox_static_proportion_lines"), "Show fold change thresholds?", value = TRUE),
                checkboxInput(ns("checkbox_static_proportion_fc_scale"), "Show ratio scale?", value = TRUE),
                selectInput(ns("select_static_proportion_sig_column"), "Sigificance column", 
                  choices = c("p-value" = "p_value", "adjusted p-value" = "adj_p_value"),
                  selected = "adj_p_value"
                )
              ),
              column(6,
                sliderInput(ns("slider_static_proportion_min"), "Proportion threshold", min = 0.01, max = 5, value = 1, step = 0.01),
                sliderInput(ns("slider_static_proportion_sig_max"), "Significance threshold", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                sliderInput(ns("slider_static_proportion_fc_min"), "Fold change threshold", min = 0.5, max = 4, value = 1, step = 0.1),
                colorPickr(ns("pick_static_proportion_positive"), "Upregulated protein color", selected = "#1e90ff"),
                colorPickr(ns("pick_static_proportion_negative"), "Downregulated protein color", selected = "#ff3030")
              )
            ),
            br(),
            fluidRow(
              column(12,
                actionButton(ns("action_replot_static_proportion"),
                  label = "Update plot"
                )
              )
            )
          ),
          div(
            id = ns("options_interactive_volcano"),
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
                    "-log10(adjusted p-value)" = "-log10(adj_p_value)",
                    "average expression" = "average_expression",
                    "proportional expression" = "proportional_expression",
                    "log10(average expression)" = "log10(average_expression)",
                    "log10(proportional expression)" = "log10(proportional_expression)"
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
        )
      ),
      
      box(
        title = "Differential expression table",
        status = "info",
        id = ns("box_table_differential_expression"),
        width = 12,
        collapsed = TRUE,
        br(),
        actionButton(ns("table_download"), 
          label = "Download table",
          icon = icon("download")
        ),
        reactableOutput(ns("table_differential_expression")) %>% withSpinner(type = 8),
        sidebar = boxSidebar(
          id = ns("box_table_differential_expression_sidebar"),
          easyClose = FALSE,
          background = "#e9e9e9",
          fluidRow(
            column(12,
              awesomeCheckbox(ns("checkbox_table_detailed"),
                label = "Display all quantitative information?",
                value = FALSE
              )
            )
          ),
          fluidRow(
            column(6,
              awesomeCheckbox(ns("checkbox_table_filter_p_values"),
                label = "Filter by adjusted p-value?",
                value = FALSE
              ),
              sliderTextInput(ns("slider_table_p_value_filter"),
                label = "accepted adj. p-values",
                choices = c(0, 1e-3, 1e-2, 5e-2, 1e-1, 1),
                selected = c(0, 5e-2)
              )
            ),
            column(6,
              awesomeCheckbox(ns("checkbox_table_filter_log2fc_values"),
                label = "Filter by log2(foldchange)?",
                value = FALSE
              ),
              sliderInput(ns("slider_table_log2fc_value_filter"),
                label = "excluded log2(foldchange) values",
                min = -4,
                max = 4,
                step = 0.1,
                value = c(-1,1),
                ticks = FALSE
              )
            )
          ),
          br(),
          fluidRow(
            column(12,
              actionButton(ns("action_filter_table"),
                label = "Update table"
              )
            )
          )
        )
      )
      
    )
  )
  
}


tab_expression_analysis_server <- function(id, tp, tp_subset, tp_normalized, tp_expression) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(input$box_plots, {
      
      if (input$box_plots == "tab_expression_analysis-box_interactive_volcano") {
        
        
        shinyjs::show("options_interactive_volcano")
        shinyjs::hide("options_static_volcano")
        shinyjs::hide("options_static_proportion")
        
      } else if (input$box_plots == "tab_expression_analysis-box_static_volcano") {
        
        shinyjs::hide("options_interactive_volcano")
        shinyjs::show("options_static_volcano")
        shinyjs::hide("options_static_proportion")
        
      } else if (input$box_plots == "tab_expression_analysis-box_static_proportion") {
        
        shinyjs::hide("options_interactive_volcano")
        shinyjs::hide("options_static_volcano")
        shinyjs::show("options_static_proportion")
        
      }
      
    })
    
    
    # Callback to download DEG table
    shinyjs::onclick("table_download", runjs(glue("Reactable.downloadDataCSV('tab_expression_analysis-table_differential_expression', '{input$select_group_one}_vs_{input$select_group_two}_expression_analysis.csv')")))
    
    
    # Disable expression tab when data are null or peptides
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp())) FALSE else if (tp()$analyte == "peptides") FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    # Disable expression analysis if single sample group available or data are null
    observe({
      
      if (is.null(tp()) | {input$select_group_one == input$select_group_two}) shinyjs::disable("action_expression_analysis") else shinyjs::enable("action_expression_analysis")
      
    })
    
    
    # Instantiate reactive for holding sample groups
    experiments <- reactiveVal(NULL)
    
    observeEvent(tp(), {
      
      if (!setequal(experiments(), tp()$experiments$sample)) experiments(tp()$experiments$sample %>% unique())
      
    })
    
    
    # Reset plotting options for new data; reveal volcano and DEG table upon input
    observeEvent(input$action_expression_analysis, {
      
      map(
        .x = c(
          "box_plots_box",
          "box_table_differential_expression"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      map(
        .x = c(
          "checkbox_table_filter_p_values",
          "slider_table_p_value_filter",
          "checkbox_table_filter_log2fc_values",
          "slider_table_log2fc_value_filter",
          "action_filter_table"
        ),
        .f = ~ shinyjs::reset(.x)
      )
        
      
    }, priority = 1)
    
    
    # Disable plotting features when deselected by user
    observe({
      
      if (input$select_volcano_color == "fixed") shinyjs::show("pick_volcano_color") else shinyjs::hide("pick_volcano_color")
      
      if (input$checkbox_table_filter_p_values) shinyjs::enable("slider_table_p_value_filter") else shinyjs::disable("slider_table_p_value_filter")
      if (input$checkbox_table_filter_log2fc_values) shinyjs::enable("slider_table_log2fc_value_filter") else shinyjs::disable("slider_table_log2fc_value_filter")
      
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
    
    
    # Update group choices if data object changes
    # Disable analysis if only one group is available
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
    
    
    # Set expression object with current tp or subsetted (pre-processed) tp object
    set_tp_expression <- eventReactive(input$action_expression_analysis, {
      
      tp_expression_analysis_annotated_filtered(NULL)
      
      if (!is.null(tp_expression())) {
        
        tp_expression(
          tp_expression() %>%
            tidyproteomics::expression(!!input$select_group_one/!!input$select_group_two,
              .method = statistical_methods[[input$select_statistical_method]]
            )
        )
        
      } else if (!is.null(tp_normalized())) {
        
        tp_expression(
          tp_normalized() %>%
            tidyproteomics::expression(!!input$select_group_one/!!input$select_group_two,
              .method = statistical_methods[[input$select_statistical_method]]
            )
        )
        
      } else if (!is.null(tp_subset())) {
        
        tp_expression(
          tp_subset() %>% 
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
      
      tp_expression()
      
    })
    
    
    # Merge annotations into DEG table if they are available
    tp_expression_analysis_annotated <- eventReactive(set_tp_expression(), {
      
      if (!is.null(tp_expression()$annotation)) {
        
        annotation_columns <- tp_expression()$annotation %>% 
          filter(term %in% c("gene_name", "description")) %>% 
          distinct(term) %>% 
          pull() %>% 
          sort(decreasing = TRUE)
        
        map(
          .x = tp_expression()$analysis, 
          .f = ~ list(
            expression = .x$expression %>% 
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
        )
        
      } else tp_expression()$analysis
      
    })
    
    
    # Instantiate reactive for holding filtered annotation table
    tp_expression_analysis_annotated_filtered <- reactiveVal(NULL)
    
    observeEvent(input$action_filter_table, {
      
      tp_expression_analysis_annotated_filtered(
        tp_expression_analysis_annotated() %>%
          pluck(glue("{input$select_group_one}/{input$select_group_two}"), "expression") %>%
          arrange(adj_p_value) %>%
          {
            if (input$checkbox_table_filter_p_values) filter(., data.table::between(.$adj_p_value, input$slider_table_p_value_filter[1], input$slider_table_p_value_filter[2])) else .
          } %>%
          {
            if (input$checkbox_table_filter_log2fc_values) filter(., !data.table::between(.$log2_foldchange, input$slider_table_log2fc_value_filter[1], input$slider_table_log2fc_value_filter[2], incbounds = FALSE)) else .
          }
      )
      
    })
    
    # Render static volcano plot
    output$plot_static_volcano_plot <- renderPlot({
      
      shiny::req(set_tp_expression())
      input$action_replot_static_volcano
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression() %>% 
          plot_volcano(!!input$select_group_one/!!input$select_group_two,
            labels_column = if ("gene_name" %in% unique(.$annotations$term)) "gene_name" else "protein",
            log2fc_min = input$slider_static_volcano_fc_min,
            significance_max = input$slider_static_volcano_sig_max,
            show_pannels = input$checkbox_static_volcano_panels,
            show_lines = input$checkbox_static_volcano_lines,
            show_fc_scale = input$checkbox_static_volcano_fc_scale,
            significance_column = input$select_static_volcano_sig_column,
            color_positive = input$pick_static_volcano_positive,
            color_negative = input$pick_static_volcano_negative,
            destination = "plot"
          )
          
        
      })
      
    })
    
    
    output$plot_static_proportion_plot <- renderPlot({
      
      shiny::req(set_tp_expression())
      input$action_replot_static_proportion
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression() %>% 
          plot_proportion(!!input$select_group_one/!!input$select_group_two,
            labels_column = if ("gene_name" %in% unique(.$annotations$term)) "gene_name" else "protein",
            log2fc_min = input$slider_static_proportion_fc_min,
            proportion_min = input$slider_static_proportion_min / 100,
            significance_max = input$slider_static_proportion_sig_max,
            show_pannels = input$checkbox_static_proportion_panels,
            show_lines = input$checkbox_static_proportion_lines,
            show_fc_scale = input$checkbox_static_proportion_fc_scale,
            significance_column = input$select_static_proportion_sig_column,
            color_positive = input$pick_static_proportion_positive,
            color_negative = input$pick_static_proportion_negative,
            destination = "plot"
          )
        
        
      })
      
    })
    
    
    # Render interactive plot
    output$plot_interactive_plot <- renderPlotly({
      
      shiny::req(set_tp_expression())
      input$action_replot_volcano
      
      isolate({
        
        shiny::req(tp_expression()$analysis)
        
        tp_expression_analysis_annotated() %>%
          pluck(glue("{input$select_group_one}/{input$select_group_two}"), "expression") %>%
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
            {if ('gene_name' %in% colnames(.)) invisible(glue('gene name: {.$gene_name}')) else 'gene name: NA'}
            {if ('description' %in% colnames(.)) invisible(glue('description: {.$description}')) else 'description: NA'}
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

    
    # Render DEG table
    output$table_differential_expression <- reactable::renderReactable({

      shiny::req(set_tp_expression())
      input$action_filter_table

      isolate({

        shiny::req(tp_expression()$analysis)
        
        {if (is.null(tp_expression_analysis_annotated_filtered())) {
          
          tp_expression_analysis_annotated() %>%
                pluck(glue("{input$select_group_one}/{input$select_group_two}"), "expression") %>%
                arrange(adj_p_value)
          
        } else {
          
          tp_expression_analysis_annotated_filtered()
          
        }} %>% 
        {
          if (!input$checkbox_table_detailed) relocate(select(., -n, -foldchange, -contains("statistic")), c("average_expression", "proportional_expression"), .after = "adj_p_value") else .
        } %>% 
          reactable(
            sortable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(
              cell = render_expression_reactable,
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
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
              ),
              log2_foldchange = colDef(
                minWidth = 120
              )
            )
          )

      })

    })
    
  })
  
}