
ui <- dashboardPage(
  
  dark = NULL,
  fullscreen = TRUE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "tidyproteomics",
      href = "https://jeffsocal.github.io/tidyproteomics/",
      color = "primary",
      image = "logo.png"
    ),
    border = FALSE,
    compact = TRUE,
    fixed = TRUE,
    div(
      style="font-weight:400;font-size:1.25rem;margin-left:7px",
      "tidyproteomics-interactive"
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 0,
    collapsed = TRUE,
    sidebarMenu(
      
      sidebarHeader("Data input and information"),
      tab_introduction_ui("tab_introduction")[["sidebar"]],
      tab_upload_data_ui("tab_upload_data")[["sidebar"]],
      
      sidebarHeader("Data preprocessing"),
      tab_subset_ui("tab_subset")[["sidebar"]],
      tab_normalize_abundances_ui("tab_normalize_abundances")[["sidebar"]],
      
      sidebarHeader("Data analysis"),
      tab_expression_analysis_ui("tab_expression_analysis")[["sidebar"]],
      tab_enrichment_analysis_ui("tab_enrichment_analysis")[["sidebar"]]
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    useShinyFeedback(),
    use_theme(
      create_theme(
        bs4dash_font(
          size_base = "0.85rem"
        ),
        bs4dash_status(
          info = "#5e8cbe7a",
          secondary = "#5ebe7c7a"
        )
      )
    ),
    tags$head(includeCSS("www/style.css")),
    tabItems(
      tab_introduction_ui("tab_introduction")[["body"]],
      tab_upload_data_ui("tab_upload_data")[["body"]],
      tab_subset_ui("tab_subset")[["body"]],
      tab_normalize_abundances_ui("tab_normalize_abundances")[["body"]],
      tab_expression_analysis_ui("tab_expression_analysis")[["body"]],
      tab_enrichment_analysis_ui("tab_enrichment_analysis")[["body"]]
    )
  ),
  footer = dashboardFooter(
    fixed = TRUE,
    left = tagList(
      tab_normalize_abundances_ui("tab_normalize_abundances")[["footer"]],
      tab_subset_ui("tab_subset")[["footer"]]
    )
  )
)