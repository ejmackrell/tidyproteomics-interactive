
server <- function(input, output, session) {
  
  # Instantiate reactives for holding tab outputs
  tp <- reactiveVal()
  tp_subset <- reactiveVal()
  tp_normalized <- reactiveVal()
  tp_expression <- reactiveVal()
  tp_enrichment <- reactiveVal()
  
  tp_collapse <- reactiveVal()
  
  # Data input and information
  tab_introduction_server("tab_introduction")
  tab_upload_data_server("tab_upload_data", tp, tp_subset, tp_normalized, tp_expression, tp_enrichment, tp_collapse)
  
  # Data preprocessing
  tab_subset_server("tab_subset", tp, tp_subset, tp_normalized)
  tab_normalize_abundances_server("tab_normalize_abundances", tp, tp_subset, tp_normalized, tp_expression)
  tab_collapse_server("tab_collapse", tp, tp_subset, tp_normalized, tp_collapse)
  
  # Data analysis
  tab_expression_analysis_server("tab_expression_analysis", tp, tp_subset, tp_normalized, tp_expression)
  tab_enrichment_analysis_server("tab_enrichment_analysis", tp, tp_expression, tp_enrichment)
  
  
  observeEvent(tp_collapse(), {
    
    updateTabItems(session, "sidebar",
      selected = "tab_upload_data"
    )
    
  })
  
}