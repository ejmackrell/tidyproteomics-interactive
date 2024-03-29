
tab_upload_data_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Data input and summary",
      tabName = "tab_upload_data",
      icon = icon("file-arrow-up")
    ),
    body = tabItem(
      tabName = "tab_upload_data",
      
      fluidRow(
        box(
          title = "Table upload",
          selectInput(ns("select_data_type"),
            label = "Select a data type",
            choices = list(
              "Proteome Discoverer" = "ProteomeDiscoverer",
              "Skyline" = "Skyline",
              "DIA-NN" = "DIA-NN",
              "MaxQuant" = "MaxQuant",
              "mzTab" = "mzTab",
              "FragPipe" = "FragPipe"
            ),
            width = "300px"
          ),
          selectInput(ns("select_analyte_type"),
            label = "Select an analyte type",
            choices = list(
              "Proteins" = "proteins",
              "Peptides" = "peptides"
            ),
            selected = "proteins",
            width = "300px"
          ),
          br(),
          uiOutput(ns("dynamic_file_input")),
          div(id = ns("div_example_data"),
            div(style='text-align:center; font-weight:600;', '—or—'),
              br(),
              checkboxInput(ns("checkbox_example_data"),
                label = "Use example dataset?",
                value = FALSE
              ),
              br()
          ),
          actionButton(ns("action_upload_table"),
            label = "Import data"
          )
        )
      ),
      
      box(
        title = "tidyproteomics object summary",
        status = "info",
        collapsed = TRUE,
        width = 10,
        id = ns("box_tidyproteomics_object_summary"),
        htmlOutput(ns("text_tidyproteomics_summary")) %>% withSpinner(type = 8, proxy.height = "250px")
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
          textInput(ns("text_contaminant_pattern"),
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
      
    )
  )
}


tab_upload_data_server <- function(id, tp, tp_subset, tp_normalized, tp_expression, tp_enrichment, tp_collapse) {
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      
      # Example dataset for PD protein-level selection
      if (input$select_data_type == "ProteomeDiscoverer") {
        
        shinyjs::show("div_example_data")
        
        # updateAwesomeCheckbox(session, inputId = "checkbox_example_data", label = "Test")
        # updateAwesomeCheckbox(session, inputId = "tab_upload_data-checkbox_example_data", label = "Test2")
        
        updateCheckboxInput(session, "checkbox_example_data", 
          label = glue("Use example {input$select_analyte_type} data?")
        )
        
      } else {
       
        shinyjs::hide("div_example_data") 
        
      }
      
      # Disable dataset selection fields if example dataset is used
      if (input$checkbox_example_data) {
        
        shinyjs::disable("select_data_type")
        shinyjs::disable("select_analyte_type")
        shinyjs::disable("dynamic_file_input")
        
      } else {
        
        shinyjs::enable("select_data_type")
        shinyjs::enable("select_analyte_type")
        shinyjs::enable("dynamic_file_input")
        
      }
      
    })
    
    observe({
      
      # Disable data upload if example dataset is used
      if (!input$checkbox_example_data) {
        
        if (is.null(input$upload_table)) shinyjs::disable("action_upload_table") else shinyjs::enable("action_upload_table")
        
      } else {
        
        shinyjs::enable("action_upload_table") 
        
      }
      
    })
    
    
    observeEvent(input$select_data_type, {
      
      # Disable protein/peptide field selection choices for certain platform choices
      if (input$select_data_type == "ProteomeDiscoverer" | 
          input$select_data_type == "MaxQuant" | 
          input$select_data_type == "mzTab" | 
          input$select_data_type == "FragPipe") {
        
        updateSelectInput(session, "select_analyte_type",
          label = "Select an analyte type",
          choices = list(
            "Proteins" = "proteins",
            "Peptides" = "peptides"
          )
        )
        
      } else if (input$select_data_type == "Skyline" | 
                 input$select_data_type == "DIA-NN") {
        
        updateSelectInput(session, "select_analyte_type",
          label = "Select an analyte type",
          choices = list(
            "Peptides" = "peptides"
          )
        )
        
      }
      
    })
    
    
    output$dynamic_file_input <- renderUI({
      
      input$select_data_type
      
      # Render file upload in platform-dependent fashion
      isolate({
      
        if (input$select_data_type == "ProteomeDiscoverer") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload a ProteomeDiscoverer search file (.xlsx)",
            accept = c(".xlsx")
          )
          
        } else if (input$select_data_type == "MaxQuant") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload a MaxQuant search file (.txt)",
            accept = c(".txt")
          )
          
        } else if (input$select_data_type == "Skyline") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload a Skyline search file (.csv)",
            accept = c(".csv")
          )
          
        } else if (input$select_data_type == "DIA-NN") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload a DIA-NN search file (.tsv)",
            accept = c(".tsv")
          )
          
        } else if (input$select_data_type == "mzTab") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload an mzTab search file (.mzTab)",
            accept = c(".mzTab")
          )
          
        } else if (input$select_data_type == "FragPipe") {
          
          fileInput("tab_upload_data-upload_table",
            label = "Upload a FragPipe search file (.tsv)",
            accept = c(".tsv")
          )
          
        }
        
      })
      
    })
    
    
    observeEvent(input$action_upload_table, {
      
      # Activate tidyproteomics object summary box after data is provided
      map(
        .x = c(
          "box_tidyproteomics_object_summary"
        ),
        .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
      )
      
      
      # Remove contaminant selection for peptide data or MQ data
      if (input$select_analyte_type == "peptides" | input$select_data_type == "MaxQuant") {
        shinyjs::hide(selector = "a[data-value='tab_upload_data-box_contaminant_selection']")
      } else {
        shinyjs::show(selector = "a[data-value='tab_upload_data-box_contaminant_selection']")
      }
      
    })
    
    
    set_tp <- eventReactive(input$action_upload_table, {
      
      # Attempt data upload
      tryCatch({
        
        if (!input$checkbox_example_data) {
      
          renamed_files <- input$upload_table %>% 
            rename_uploaded_file()
          
          tp(
            tidyproteomics::import(
              files = renamed_files$datapath,
              platform = input$select_data_type,
              analyte = input$select_analyte_type
            )
          )
          
          # Reset subsetted and normalized objects upon upload of new data
          tp_subset(NULL)
          tp_normalized(NULL)
          tp_expression(NULL)
          tp_enrichment(NULL)
          
          tp()
          
        } else {
          
          if (input$select_analyte_type == "proteins") {
          
            tp(
              hela_proteins
            )
            
            # Reset subsetted and normalized objects upon upload of new data
            tp_subset(NULL)
            tp_normalized(NULL)
            tp_expression(NULL)
            tp_enrichment(NULL)
            
            tp()
            
          } else {
            
            tp(
              hela_peptides
            )
            
            # Reset subsetted and normalized objects upon upload of new data
            tp_subset(NULL)
            tp_normalized(NULL)
            tp_expression(NULL)
            tp_enrichment(NULL)
            
            tp()
            
          }
          
        }
        
      },
        error = function(e) {
          return("Failed to import table. Please verify the platform and analyte type match those of the table, and please ensure there are no special characters (e.g., hyphens or slashes) in the sample names.")
        }
      )
        
      
    })
    
    
    set_tp_from_collapse <- eventReactive(tp_collapse(), {
      
      if (!is.null(tp_collapse())) {
      
        tp(
          tp_collapse()
        )
      
        if (tp()$origin == "MaxQuant") {
          
          shinyjs::hide(selector = "a[data-value='tab_upload_data-box_contaminant_selection']")
          
        } else {
          
          shinyjs::show(selector = "a[data-value='tab_upload_data-box_contaminant_selection']")
          
        }
        
        tp()
          
      } else NULL
      
    }, ignoreNULL = FALSE)
    
    
    output$text_tidyproteomics_summary <- renderUI({
      
      shiny::req(set_tp())
      set_tp_from_collapse()
      
      validate(
        need(!is.character(set_tp()), set_tp())
      )

      isolate({
        
        # Toggle data summarization box
        map(
          .x = c(
            "tabbox_data_summary_box"
          ),
          .f = ~ if (input[[.x]]$collapsed) updateBox(.x, action = 'toggle')
        )
        
        # Print tidyproteomics object summary
        HTML(paste0("<pre style='padding: 0rem; margin-bottom: 0rem; overflow: hidden;'>", capture.output(tp()), "</pre>"))
        
      })

    })
    
    
    observeEvent(tp(), {
      
      # Update summary variables with available values from tp object
      updateSelectInput(session, "select_summary_table",
        label = "Select a summary variable",
        choices = tidyproteomics:::get_variables(tp())
      )
      
      updateSelectInput(session, "select_data_feature",
        label = "Select a data feature",
        choices = tabular_data_features[tabular_data_features %in% names(tp())]
      )
      
    })
    
    
    # Render data feature table upon selection
    output$table_data_feature <- reactable::renderReactable({
      
      shiny::req(input$action_view_feature)
      
      isolate({
        
        shiny::req(tp())
        
        tp() %>% 
          pluck(input$select_data_feature) %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            defaultColDef = colDef(
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
            ),
            columns = build_summary_col_defs(.)
          )
        
      })
      
    })
    
    
    # Render summary feature table upon selection
    output$table_summary_sample <- reactable::renderReactable({
      
      shiny::req(input$action_summarize)
      
      isolate({
      
        shiny::req(tp())
        
        tp() %>% 
          summary(input$select_summary_table, destination = "return") %>% 
          reactable(
            sortable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            defaultColDef = colDef(
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
            ),
            columns = build_summary_col_defs(.)
          )
        
      })
          
      
    })
    
    
    # Render contamination table upon selection
    output$table_summary_contamination <- reactable::renderReactable({
      
      shiny::req(input$action_contaminant_summarize)
      
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
            resizable = TRUE,
            defaultColDef = colDef(
              sortNALast = TRUE,
              headerStyle = list(background = "#FAFAFA")
            ),
            columns = build_summary_col_defs(.)
          )
      })
      
    })
    
  })
  
}