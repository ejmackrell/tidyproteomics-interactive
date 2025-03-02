
ui <- dashboardPage(
  
  dark = NULL,
  help = NULL,
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
      style="font-weight:400;font-size:1.25rem;margin-left:7px;margin-right:7px",
      "tidyproteomics-interactive"
    ),
    span(class="left badge badge-secondary badge-pill",style="background: lightgrey; font-size: 0.75rem; font-weight: 400", "v1.2.1"),
    rightUi = shiny::tags$li(
      class = "dropdown",
      a(
        href = "https://ejmackrell.github.io/tidyproteomics-interactive/", target = "_blank",
        # icon("circle-question", style = "color: #777; margin-right: 10px", class="fa-2xl fa-solid")
        bsicons::bs_icon("question-circle", color = "#777", size = "2em", margin_right = "10px")
      ),
      a(
        href = "https://github.com/ejmackrell/tidyproteomics-interactive", target = "_blank",
        # icon("github", style = "color: #777", class="fa-2xl")
        bsicons::bs_icon("github", color = "#777", size = "2em")
      )
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 0,
    collapsed = TRUE,
    sidebarMenu(
      
      id = "sidebar",
      
      sidebarHeader("Data input and information"),
      tab_introduction_ui("tab_introduction")[["sidebar"]],
      tab_upload_data_ui("tab_upload_data")[["sidebar"]],
      
      sidebarHeader("Data preprocessing"),
      tab_subset_ui("tab_subset")[["sidebar"]],
      tab_normalize_abundances_ui("tab_normalize_abundances")[["sidebar"]],
      tab_collapse_ui("tab_collapse")[["sidebar"]],
      
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
      tab_collapse_ui("tab_collapse")[["body"]],
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