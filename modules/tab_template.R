
tab_name_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Tab title",
      tabName = "tab_name",
      icon = icon("list")
    ),
    body = tabItem(
      tabName = "tab_name"
    )
  )
  
}

tab_name_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
  })
  
}