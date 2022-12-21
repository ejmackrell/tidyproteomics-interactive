
tab_subset_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Data subsetting and summary",
      tabName = "tab_subset",
      icon = icon("filter"),
      condition = "output['tab_subset-tab_subset_availability'] == true"
    ),
    body = tabItem(
      tabName = "tab_subset",
      
      box(
        title = "Subsetting parameters",
        width = 12,
        id = ns("box_subsetting_parameters"),
        awesomeCheckbox(ns("checkbox_enable_subsetting"),
          label = "Subset data by user pattern?",
          value = TRUE
        ),
        div(
          id = ns("group_subsetting_parameters"),
          fluidRow(
            column(4,
              selectInput(ns("select_subsetting_variable"),
                label = "Please upload a data table to use this feature",
                choices = NULL
              )
            ),
            column(4,
              selectInput(ns("select_operator"),
                label = "Please upload a data table to use this feature",
                choices = NULL
              )
            ),
            column(4,
              shinyjs::hidden(
                textInput(ns("text_subsetting_value"),
                  label = "Provide a value for subsetting"
                ),
                selectInput(ns("select_logical_subsetting_value"),
                  label = "Select a value for subsetting",
                  choices = c(TRUE, FALSE)
                )
              )
            )
          )
        ),
        br(),
        awesomeCheckbox(ns("checkbox_subset_contamination"),
          label = "Remove contamination by subsetting with pattern?",
          value = TRUE
        ),
        textInput(ns("text_contaminant_pattern"),
          label = "Provide a contaminant pattern",
          placeholder = "e.g., CRAP",
          width = "300px"
        ),
        br(),
        shinyjs::disabled(
          actionButton(ns("action_subset"),
            label = "Subset data"
          )
        )
      ),
      
      box(
        title = "Subsetted data feature selection",
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
        title = "Subsetted data feature viewer",
        status = "info",
        collapsed = TRUE,
        width = 12,
        id = ns("box_data_feature_viewer"),
        reactableOutput(ns("table_data_feature")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Subsetted data summary table selection",
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
        title = "Subsetted data summary statistics",
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
        textInput(ns("text_subsetted_contaminant_pattern"),
          label = "Provide a contaminant pattern",
          placeholder = "e.g., CRAP"
        ),
        actionButton(ns("action_contaminant_summarize"),
          label = "Evaluate contaminant pattern"
        )
      ),
      
      box(
        title = "Subsetted data contamination statistics",
        status = "info",
        id = ns("box_contamination_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_contamination")) %>% withSpinner(type = 8)
      )
    ),
    
    footer = tagList(
      htmlOutput(ns("subset_output")),
      htmlOutput(ns("contamination_output"))
    )
  )
  
}

tab_subset_server <- function(id, tp, tp_subset, tp_normalized) {
  
  moduleServer(id, function(input, output, session) {
    
    
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp())) FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    observeEvent(input$checkbox_enable_subsetting, {
      
      if (input$checkbox_enable_subsetting) {
        shinyjs::show("group_subsetting_parameters")
      } else {
        shinyjs::hide("group_subsetting_parameters")
      }
      
    })
    
    
    observeEvent(input$checkbox_subset_contamination, {
      
      if (input$checkbox_subset_contamination) {
        shinyjs::show("text_contaminant_pattern")
      } else {
        shinyjs::hide("text_contaminant_pattern")
      }
      
    })
    
    
    tp_subset_variables <- eventReactive(tp(), {
      
      list(
        experiments = tp()$experiments %>% names(), 
        accounting = tp()$accounting %>% names(), 
        annotations = tp()$annotations$term %>% unique()
      ) %>% 
        tibble::enframe() %>% 
        tidyr::unnest(value) %>% 
        mutate(
          type = map2(
            .x = name,
            .y = value,
            .f = ~ {
              if (.x == "annotations") {
                tp()[["annotations"]]$annotation %>% typeof()
              } else {
                tp()[[.x]][[.y]] %>% class()
              }
            }
          ) %>% unlist()
        ) %>% 
        mutate(id = row_number())
      
    })
    
    
    observeEvent(input$action_summarize, {
      
      map(
        .x = c(
          "box_summary_statistics"
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
      
      updateSelectInput(session, "select_subsetting_variable",
        label = "Select a variable for subsetting",
        choices = tp_subset_variables() %>% 
          mutate(id = row_number()) %>% 
          split(.$name) %>% 
          map(
            .f = ~ .x %>% 
              select(value, id) %>% 
              tibble::deframe()
          )
      )
      
      if (tp()$analyte == "peptides") {
        
        updateCheckboxInput(session, "checkbox_subset_contamination",
          label = "Remove contamination by subsetting with pattern?",
          value = FALSE
        )
        
        map(
          .x = c(
            "checkbox_subset_contamination",
            "text_contaminant_pattern",
            "contamination_output"
          ),
          .f = ~ shinyjs::hide(.x)
        )
        
        map(
          .x = c(
            "box_contaminant_selection",
            "box_contamination_statistics"
          ),
          .f = ~ updateBox(.x, "remove")
        )
        
      } else {
        
        updateCheckboxInput(session, "checkbox_subset_contamination",
          label = "Remove contamination by subsetting with pattern?",
          value = TRUE
        )
        
        map(
          .x = c(
            "checkbox_subset_contamination",
            "text_contaminant_pattern",
            "contamination_output"
          ),
          .f = ~ shinyjs::show(.x)
        )
        
        map(
          .x = c(
            "box_contaminant_selection",
            "box_contamination_statistics"
          ),
          .f = ~ updateBox(.x, "restore")
        )
        
      }
      
    })
    
    
    observeEvent(input$select_subsetting_variable, {
      
      shiny::req(tp())
      
      subset_variable_type <- tp_subset_variables() %>% 
        filter(id == input$select_subsetting_variable) %>% 
        pull(type)
      
      updateSelectInput(session, "select_operator",
        choices = subsetting_operators[[subset_variable_type]],
        label = "Select an operator for comparison"
      )
      
      if (subset_variable_type == "logical") {
        
        shinyjs::hide("text_subsetting_value")
        shinyjs::show("select_logical_subsetting_value")
        
      } else {

        shinyjs::show("text_subsetting_value")
        shinyjs::hide("select_logical_subsetting_value")
        
      }
      
    })
    
    
    
    
    observe({
      
      shiny::req(tp())
      
      subset_variable_type <- tp_subset_variables() %>% 
        filter(id == input$select_subsetting_variable) %>% 
        pull(type)
      
      # browser()
      
      shiny::req(subset_variable_type)
      
      
      if(
        {
          input$checkbox_enable_subsetting &
          subset_variable_type == "character" &
          input$text_subsetting_value == ""
        } |
        {
          input$checkbox_subset_contamination &
          input$text_contaminant_pattern == ""
        } |
        {
          !input$checkbox_enable_subsetting &
          !input$checkbox_subset_contamination
        }
      ) shinyjs::disable("action_subset") else shinyjs::enable("action_subset")
      
      if (input$text_subsetted_contaminant_pattern == "") shinyjs::disable("action_contaminant_summarize") else shinyjs::enable("action_contaminant_summarize")
      
    })
    

    
    observeEvent(input$text_subsetting_value, {
      
      if (input$text_subsetting_value == "") {
        
        showFeedbackWarning("text_subsetting_value", text = NULL, icon = NULL)
        
      } else {
        
        hideFeedback("text_subsetting_value")
        
      }
      
    })
    
    observeEvent(input$text_contaminant_pattern, {
      
      if (input$text_contaminant_pattern == "") {
        
        showFeedbackWarning("text_contaminant_pattern", text = NULL, icon = NULL)
        
      } else {
        
        hideFeedback("text_contaminant_pattern")
        
      }
      
    })
    
    
    set_tp_subset <- eventReactive(input$action_subset, {
      
      chosen_subset_variable <- tp_subset_variables() %>% 
        filter(id == input$select_subsetting_variable) %>% 
        pull(value)
      
      subset_variable_type <- tp_subset_variables() %>% 
        filter(id == input$select_subsetting_variable) %>% 
        pull(type)
      
      # browser()
      
      tp_subset(
        tp() %>%
          {
            if (input$checkbox_enable_subsetting) {
              
              if (subset_variable_type == "logical") {
                
                subset(., !!paste(chosen_subset_variable, input$select_operator, input$select_logical_subsetting_value))
                
              } else {
                
                if (input$select_operator == "! %like%") {
                  
                  subset(., !!paste(paste0("!",chosen_subset_variable), "%like%", input$text_subsetting_value))
                  
                } else {
                  
                  subset(., !!paste(chosen_subset_variable, input$select_operator, input$text_subsetting_value))
                  
                }
              }
            } else .
          } %>% 
          {
            if (input$checkbox_subset_contamination) {
              subset(., !description %like% !!input$text_contaminant_pattern)
            } else .
          }
      )
      
      tp_normalized(NULL)
      tp_subset()
      
    })
    
    observeEvent(input$action_subset, {
      
      map(
        .x = c(
          "box_summary_table_selection",
          "box_contaminant_selection",
          "box_data_feature_selection"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )

    })
    
    
    observeEvent(set_tp_subset(), {
      
      updateSelectInput(session, "select_summary_table",
        label = "Select a summary variable",
        choices = tidyproteomics:::get_variables(tp_subset())
      )
      
      updateSelectInput(session, "select_data_feature",
        label = "Select a data feature",
        choices = tabular_data_features[tabular_data_features %in% names(tp_subset())]
      )
      
    })

    
    output$table_data_feature <- reactable::renderReactable({
      
      input$action_view_feature
      
      isolate({
        
        shiny::req(set_tp_subset())
        
        tp_subset() %>% 
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
        
        shiny::req(set_tp_subset())
        
        tp_subset() %>% 
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
    
    
    observeEvent(input$action_contaminant_summarize, {
      
      map(
        .x = c(
          "box_contamination_statistics"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
    })
    
    
    output$table_summary_contamination <- reactable::renderReactable({
      
      input$action_contaminant_summarize
      
      isolate({
        
        shiny::req(set_tp_subset())
        
        # browser()
        
        tp_subset() %>% 
          summary(
            contamination = input$text_subsetted_contaminant_pattern,
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
    
    
  
    
    
    subset_statements <- eventReactive(input$action_subset, {
      
      subsetting_operations <- tp_subset()$operations[!{tp_subset()$operations %>% stringr::str_match("subset") %>% is.na()}]

      
      subsetting_operations <- if (length(subsetting_operations) == 2) {
        
        setNames(subsetting_operations, c("subset", "contamination"))
        
      } else if (input$checkbox_enable_subsetting) {
        
        setNames(subsetting_operations, "subset") 
        
      } else if (input$checkbox_subset_contamination) {
        
        setNames(subsetting_operations, "contamination")
        
      }
      
      subsetting_operations
      
    })
    
    
    output$contamination_output <- renderUI({
      
      shiny::req(tp())
      
      # browser()
      
      if (!is.null(tp_subset())) {
        
        if (!is.null(subset_statements()$contamination)) {
          
          display_contamination_html <- glue('<span style="font-weight:300;font-size:0.75rem;">{subset_statements()$contamination}</span>')
          HTML(glue('<span class="footer-information">contamination removed: true<br>{display_contamination_html}</span>'))
          
        } else {
          
          HTML('<span class="footer-information-warning">contamination removed: false</span>')
          
        }
        
      } else {
        
        # HTML('<span style="font-weight:400;font-size:1rem;margin-left:7px;border-left: black;border-left-width: thin;border-left-style: solid;padding-left: 7px;float: right;">subsetted: false</span>')
        HTML('<span class="footer-information-warning">contamination removed: false</span>')
        
      }

      # HTML('<span style="font-weight:400;font-size:1rem;margin-left:7px;border-left: black;border-left-width: thin;border-left-style: none;padding-left: 7px;float: right;">contamination removal: true</span>')

    })

    
    output$subset_output <- renderUI({

      shiny::req(tp())

      # browser()
      
      if (!is.null(tp_subset())) {

        if (!is.null(subset_statements()$subset)) {
  
          display_subset_html <- glue('<span style="font-weight:300;font-size:0.75rem;">{subset_statements()$subset}</span>')
          HTML(glue('<span class="footer-information">subsetted: true<br>{display_subset_html}</span>'))
  
        } else {
          
          HTML('<span class="footer-information-warning">subsetted: false</span>')
          
        }
        
      } else {
  
          # HTML('<span style="font-weight:400;font-size:1rem;margin-left:7px;border-left: black;border-left-width: thin;border-left-style: solid;padding-left: 7px;float: right;">subsetted: false</span>')
          HTML('<span class="footer-information-warning">subsetted: false</span>')
  
      }

    })
    
  })
  
}