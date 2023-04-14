
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
        
        awesomeCheckbox(ns("checkbox_reassign"),
          label = "Reassign sample groups?",
          value = FALSE
        ),
        rhandsontable::rHandsontableOutput(ns("table_reassign_samples")),
        br(),
        
        shinyjs::disabled(
          actionButton(ns("action_subset"),
            label = "Subset data"
          )
        )
      ),
      
      tabBox(
        id = ns("tabbox_data_summary"),
        type = "tabs",
        width = 12,
        status = "info",
        collapsed = TRUE,
        tabPanel(
          title = "Data feature selection",
          collapsed = TRUE,
          value = ns("box_data_feature_selection"),
          selectInput(ns("select_data_feature"),
            label = "Please upload a data table to use this feature",
            choices = NULL
          ),
          actionButton(ns("action_view_feature"),
            label = "View feature"
          ),
          br(),
          reactableOutput(ns("table_data_feature")) %>% withSpinner(type = 8)
        ),
        tabPanel(
          title = "Summary table selection",
          collapsed = TRUE,
          value = ns("box_summary_table_selection"),
          selectInput(ns("select_summary_table"),
            label = "Please upload a data table to use this feature",
            choices = NULL
          ),
          actionButton(ns("action_summarize"),
            label = "Summarize data"
          ),
          br(),
          reactableOutput(ns("table_summary_sample")) %>% withSpinner(type = 8)
        ),
        tabPanel(
          title = "Contaminant selection",
          value = ns("box_contaminant_selection"),
          collapsed = TRUE,
          textInput(ns("text_subsetted_contaminant_pattern"),
            label = "Provide a contaminant pattern",
            placeholder = "e.g., CRAP"
          ),
          actionButton(ns("action_contaminant_summarize"),
            label = "Evaluate contaminant pattern"
          ),
          br(),
          reactableOutput(ns("table_summary_contamination")) %>% withSpinner(type = 8)
        )
      ),
      HTML(rep("<br>", 34) %>% glue_collapse())
      
    ),
    
    footer = tagList(
      htmlOutput(ns("subset_output")),
      htmlOutput(ns("contamination_output"))
    )
  )
  
}

tab_subset_server <- function(id, tp, tp_subset, tp_normalized) {
  
  moduleServer(id, function(input, output, session) {
    
    # Hide tab when no data is available
    output$tab_subset_availability <- reactive({  
      
      if (is.null(tp())) FALSE else TRUE
      
    })
    
    outputOptions(output, "tab_subset_availability", suspendWhenHidden = FALSE)
    
    
    # Disable subsetting fields if subsetting deselected
    observeEvent(input$checkbox_enable_subsetting, {
      
      if (input$checkbox_enable_subsetting) {
        shinyjs::enable("group_subsetting_parameters")
      } else {
        shinyjs::disable("group_subsetting_parameters")
      }
      
    })
    
    
    # Disable contaminant removal if contaminant removal deselected
    observeEvent(input$checkbox_subset_contamination, {
      
      if (input$checkbox_subset_contamination) {
        shinyjs::enable("text_contaminant_pattern")
      } else {
        shinyjs::disable("text_contaminant_pattern")
      }
      
    })
    
    
    # Disable sample group reassignment if reassignment deselected
    observeEvent(input$checkbox_reassign, {
      
      if (input$checkbox_reassign) {
        shinyjs::enable("table_reassign_samples")
      } else {
        shinyjs::disable("table_reassign_samples")
      }
      
      
    })
    
    
    # Extract all fields that can be used for subsetting the data
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
    
    
    # Render Excel-like table for reassigning sample groups
    output$table_reassign_samples <- renderRHandsontable({
      
      shiny::req(tp())
      
      isolate({
        
        rhandsontable(
          data = tp() %>% 
            pluck("experiments") %>% 
            select(sample_id, sample_file, sample) %>% 
            mutate(new_sample_name = "")
        ) %>% 
          hot_col(c("sample_id", "sample_file", "sample"),
            readOnly = TRUE
          )
        
      })
      
      
    })
    
    
    observeEvent(tp(), {
      
      # Update subsetting options
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
      
      # Disable contaminant removal option if MQ data
      if (tp()$analyte == "peptides" | tp()$origin == "MaxQuant") {
        
        shinyjs::hide(selector = "a[data-value='tab_subset-box_contaminant_selection']")
        
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
        
      } else {
        
        shinyjs::show(selector = "a[data-value='tab_subset-box_contaminant_selection']")
        
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
        
      }
      
    })
    
    
    observeEvent(input$select_subsetting_variable, {
      
      shiny::req(tp())
      
      # Extract type for chosen subsetting variable (quant/qual/logical)
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
      
      # Extract type for chosen subsetting variable (quant/qual/logical)
      subset_variable_type <- tp_subset_variables() %>%
        filter(id == input$select_subsetting_variable) %>%
        pull(type)

      shiny::req(subset_variable_type)
      shiny::req(input$table_reassign_samples$data)

      # Disable subsetting/contaminant removal/reassignemnt for invalid/incomplete control choices
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
          
          input$checkbox_reassign &
          "" %in% {input$table_reassign_samples$data %>% 
              map_df(
                .f = ~ tibble::as_tibble(.x, 
                  .name_repair = ~{input$table_reassign_samples$params$rColnames %>% 
                      as.character()}
                )
              ) %>% 
              pull(new_sample_name)}
        } |
        {
          !input$checkbox_enable_subsetting &
          !input$checkbox_subset_contamination &
          !input$checkbox_reassign
        }
      ) shinyjs::disable("action_subset") else shinyjs::enable("action_subset")

      
      if (input$text_subsetted_contaminant_pattern == "") shinyjs::disable("action_contaminant_summarize") else shinyjs::enable("action_contaminant_summarize")

    })

    
    observe({
      
      # Render feedback warnings for empty required fields
      if (input$checkbox_enable_subsetting & input$text_subsetting_value == "") {
        
        showFeedbackWarning("text_subsetting_value", text = NULL, icon = NULL)
        
      } else {
        
        hideFeedback("text_subsetting_value")
        
      }
      
      
      if (input$checkbox_subset_contamination & input$text_contaminant_pattern == "") {
        
        showFeedbackWarning("text_contaminant_pattern", text = NULL, icon = NULL)
        
      } else {
        
        hideFeedback("text_contaminant_pattern")
        
      }
      
      
    })


    set_tp_subset <- eventReactive(input$action_subset, {
      
      # Extract chosen subsetting variable and its type
      chosen_subset_variable <- tp_subset_variables() %>%
        filter(id == input$select_subsetting_variable) %>%
        pull(value)

      subset_variable_type <- tp_subset_variables() %>%
        filter(id == input$select_subsetting_variable) %>%
        pull(type)

      # Reassign sample groups and/or subset and/or remove contaminants from the current tp object
      tp_subset(
        tp() %>%
          {
            
            if (input$checkbox_reassign) {
              
              df_sample_names <- input$table_reassign_samples$data %>% 
                map_df(
                  .f = ~ tibble::as_tibble(.x, 
                    .name_repair = ~ {input$table_reassign_samples$params$rColnames %>% 
                        as.character()}
                  )
                ) 
              
              tp() %>% 
                reduce2(
                  .x = df_sample_names$sample_id,
                  .y = df_sample_names$new_sample_name,
                  .f = ~ reassign(.x, 
                    field = "sample_id",
                    pattern = ..2,
                    replace = ..3
                  ),
                  .init = .
                )
              
            } else .
            
          } %>% 
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
      
      # Toggle tp object summary box after subsetting
      map(
        .x = c(
          "tabbox_data_summary_box"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )

    })


    observeEvent(set_tp_subset(), {
      
      # Update summary variable and data feature selection values
      updateSelectInput(session, "select_summary_table",
        label = "Select a summary variable",
        choices = tidyproteomics:::get_variables(tp_subset())
      )

      updateSelectInput(session, "select_data_feature",
        label = "Select a data feature",
        choices = tabular_data_features[tabular_data_features %in% names(tp_subset())]
      )

    })

    
    # Render data feature table
    output$table_data_feature <- reactable::renderReactable({

      shiny::req(input$action_view_feature)

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
            ),
            columns = build_summary_col_defs(.)
          )

      })

    })

    
    # Render summary table
    output$table_summary_sample <- reactable::renderReactable({

      shiny::req(input$action_summarize)

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
            columns = build_summary_col_defs(.)
          )

      })


    })


    # Render contaminant table
    output$table_summary_contamination <- reactable::renderReactable({

      shiny::req(input$action_contaminant_summarize)

      isolate({

        shiny::req(set_tp_subset())

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
            resizable = TRUE,
            columns = build_summary_col_defs(.)
          )
      })

    })




    # Store operations in reactive for display to user
    subset_statements <- eventReactive(input$action_subset, {

      subsetting_operations <- tp_subset()$operations[!{tp_subset()$operations %>% stringr::str_match("subset") %>% is.na()}]


      subsetting_operations <- if (length(subsetting_operations) == 2) {

        setNames(subsetting_operations, c("subset", "contamination"))

      } else if (input$checkbox_enable_subsetting & length(subsetting_operations) == 1) {

        setNames(subsetting_operations, "subset")

      } else if (input$checkbox_subset_contamination & length(subsetting_operations) == 1) {

        setNames(subsetting_operations, "contamination")

      }

      subsetting_operations

    })

    
    # Render contamination indicator in footer
    output$contamination_output <- renderUI({

      shiny::req(tp())

      if (!is.null(tp_subset())) {

        if (!is.null(subset_statements()$contamination)) {

          display_contamination_html <- glue('<span style="font-weight:300;font-size:0.75rem;">{subset_statements()$contamination}</span>')
          HTML(glue('<span class="footer-information">contamination removed: true<br>{display_contamination_html}</span>'))

        } else {

          HTML('<span class="footer-information-warning">contamination removed: false</span>')

        }

      } else {

        HTML('<span class="footer-information-warning">contamination removed: false</span>')

      }

    })

    
    # Render subsetting indicator in footer
    output$subset_output <- renderUI({

      shiny::req(tp())

      if (!is.null(tp_subset())) {

        if (!is.null(subset_statements()$subset)) {

          display_subset_html <- glue('<span style="font-weight:300;font-size:0.75rem;">{subset_statements()$subset}</span>')
          HTML(glue('<span class="footer-information">subsetted: true<br>{display_subset_html}</span>'))

        } else {

          HTML('<span class="footer-information-warning">subsetted: false</span>')

        }

      } else {

          HTML('<span class="footer-information-warning">subsetted: false</span>')

      }

    })
    
  })
  
}