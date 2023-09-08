
tab_collapse_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Peptide collapse",
      tabName = "tab_collapse",
      icon = icon("minimize"),
      condition = "output['tab_collapse-tab_collapse_availability'] == true"
    ),
    body = tabItem(
      tabName = "tab_collapse",
      
      box(
        title = "Collapse parameters",
        
        selectInput(ns("select_assignment_method"),
          label = "Select assignment method",
          width = "300px",
          choices = list(
            "all-possible",
            "razor-local",
            "razor-global",
            "non-homologous"
          ),
          multiple = FALSE
        ),
        br(),
        actionButton(ns("action_collapse"),
          label = "Collapse peptide data"
        )
      )
    )
  )
  
}


tab_collapse_server <- function(id, tp, tp_subset, tp_normalized, tp_collapse) {
  
  moduleServer(id, function(input, output, session) {
    
    # Hide tab when no data is available
    output$tab_collapse_availability <- reactive({  
      
      if (is.null(tp())) {
        
        FALSE
        
      } else { 
        
        if (tp()$analyte == "peptides") {
          
          TRUE
          
        } else FALSE
        
      }
      
    })
    
    outputOptions(output, "tab_collapse_availability", suspendWhenHidden = FALSE)
    
    
    observeEvent(input$action_collapse, {
      
      # Set tp object from collapsed subset object if available; else use unprocessed object
      if (!is.null(tp_subset())) {
        
        tp_collapse(
          collapse(tp_subset(), 
            assign_by = input$select_assignment_method
          )
        )
        
      } else {
        
        tp_collapse(
          collapse(tp(), 
            assign_by = input$select_assignment_method
          )
        )
        
      }
      
      # Clear objects
      tp_subset(NULL)
      tp_normalized(NULL)
      
    })
    
  })
  
}