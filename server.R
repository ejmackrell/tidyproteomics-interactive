
server <- function(input, output, session) {
  
  tp <- reactiveVal()
  tp_subset <- reactiveVal()
  tp_normalized <- reactiveVal()
  tp_expression <- reactiveVal()
  tp_enrichment <- reactiveVal()
  
  tab_introduction_server("tab_introduction")
  tab_upload_data_server("tab_upload_data", tp, tp_subset, tp_normalized, tp_expression, tp_enrichment)
  
  tab_subset_server("tab_subset", tp, tp_subset, tp_normalized)
  tab_normalize_abundances_server("tab_normalize_abundances", tp, tp_subset, tp_normalized)
  
  tab_expression_analysis_server("tab_expression_analysis", tp, tp_subset, tp_normalized, tp_expression)
  tab_enrichment_analysis_server("tab_enrichment_analysis", tp, tp_expression, tp_enrichment)
  
}