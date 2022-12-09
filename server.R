
server <- function(input, output, session) {
  
  tp <- reactiveVal()
  
  tab_upload_data_server("tab_upload_data", tp)
  tab_normalize_abundances_server("tab_normalize_abundances", tp)
  tab_expression_analysis_server("tab_expression_analysis", tp)
  
}