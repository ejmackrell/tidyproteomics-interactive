
ui <- dashboardPage(
  
  dark = NULL,
  fullscreen = TRUE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "tidyproteomics",
      # href = "https://github.com/jeffsocal/tidyproteomics",
      href = "https://jeffsocal.github.io/tidyproteomics/",
      color = "primary",
      image = "logo.png"
    ),
    border = FALSE,
    compact = TRUE,
    div(
      style="font-weight:400;font-size:1.25rem;margin-left:7px",
      "tidyproteomics.interactive"
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 0,
    sidebarMenu(
      tab_upload_data_ui("tab_upload_data")[["sidebar"]],
      tab_normalize_abundances_ui("tab_normalize_abundances")[["sidebar"]],
      tab_expression_analysis_ui("tab_expression_analysis")[["sidebar"]]
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    use_theme(
      create_theme(
        bs4dash_font(
          size_base = "0.85rem"
        )
      )
    ),
    tags$head(includeCSS("www/style.css")),
    tabItems(
      tab_upload_data_ui("tab_upload_data")[["body"]],
      tab_normalize_abundances_ui("tab_normalize_abundances")[["body"]],
      tab_expression_analysis_ui("tab_expression_analysis")[["body"]]
    )
  ),
  footer = dashboardFooter(
    left = "Created by Elliot MacKrell"
  )
)