
server <- function(input, output, session) {
  
  tp <- reactiveVal()
  tp_normalized <- reactiveVal()
  tp_expression <- reactiveVal()
  
  tab_upload_data_server("tab_upload_data", tp)
  tab_normalize_abundances_server("tab_normalize_abundances", tp, tp_normalized)
  tab_expression_analysis_server("tab_expression_analysis", tp, tp_normalized, tp_expression)
  
}