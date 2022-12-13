
tab_subset_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Data subsetting and summary",
      tabName = "tab_subset",
      icon = icon("filter")
    ),
    body = tabItem(
      tabName = "tab_subset",
      
      box(
        title = "Subsetting parameters",
        width = 12,
        id = ns("box_subsetting_parameters"),
        fluidRow(
          column(3,
            selectInput(ns("select_subsetting_variable"),
              label = "Please upload a data table to use this feature",
              choices = NULL
            )
          ),
          column(3,
            selectInput(ns("select_operator"),
              label = "Please upload a data table to use this feature",
              choices = NULL
            )
          ),
          column(3,
            textInput(ns("text_subsetting_value"),
              label = "Please upload a data table to use this feature",
              placeholder = "Ribosome"
            )
          )
        ),
        actionButton(ns("action_subset"),
          label = "Subset data"
        )
      ),
      
      box(
        title = "Summary table selection",
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
        title = "Summary statistics",
        id = ns("box_summary_statistics"),
        collapsed = TRUE,
        width = 12,
        reactableOutput(ns("table_summary_sample")) %>% withSpinner(type = 8)
      ),
      
      box(
        title = "Contaminant selection",
        id = ns("box_contaminant_selection"),
        collapsed = TRUE,
        textInput(ns("text_contaminant_pattern"),
          label = "Provide a contaminant pattern",
          placeholder = "e.g., CRAP"
        ),
        actionButton(ns("action_contaminant_summarize"),
          label = "Evaluate contaminant pattern"
        )
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

tab_subset_server <- function(id, tp, tp_subset) {
  
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(input$action_subset, {
      
      map(
        .x = c(
          "box_summary_table_selection",
          "box_contaminant_selection"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      # browser()
      
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
      
      
      updateSelectInput(session, "select_summary_table",
        label = "Select a summary variable",
        choices = tidyproteomics:::get_variables(tp())
      )
      
    })
    
    
    observeEvent(input$select_subsetting_variable, {
      
      shiny::req(tp())
      
      # browser()
      
      subset_variable_type <- tp_subset_variables() %>% 
        filter(id == input$select_subsetting_variable) %>% 
        pull(type)
      
      updateSelectInput(session, "select_operator",
        choices = subsetting_operators[[subset_variable_type]],
        label = "Select an operator for comparison"
      )
      
    })
    
    
    # tp_summary <- eventReactive(input$action_upload_table, {
    #   
    #   shiny::req(tp())
    #   
    #   tidyproteomics:::get_variables(tp()) %>% 
    #     setNames(.,.) %>% 
    #     map(
    #       .x = .,
    #       .f = ~ summary(tp(), .x, destination = "return")
    #     )
    #   
    # })
    # 
    
    output$table_summary_sample <- reactable::renderReactable({
      
      input$action_summarize
      
      isolate({
        
        # shiny::req(tp_summary())
        shiny::req(tp())
        
        # tp_summary() %>% 
        # pluck(input$select_summary_table) %>% 
        tp %>% 
          summary(input$select_summary_table, destination = "return") %>% 
          reactable(
            elementId = "test_reactable",
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
    
    
    output$table_summary_contamination <- reactable::renderReactable({
      
      input$action_contaminant_summarize
      
      isolate({
        
        shiny::req(tp())
        
        tp() %>% 
          summary(
            contamination = input$text_contaminant_pattern,
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