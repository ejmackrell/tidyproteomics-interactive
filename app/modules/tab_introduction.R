
tab_introduction_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebar = menuItem("Application introduction",
      tabName = "tab_introduction",
      icon = icon("info")
    ),
    body = tabItem(
      tabName = "tab_introduction",
      
      box(
        title = "Application summary",
        status = "info",
        id = ns("box_summary"),
        width = 6,
        tags$div(
          tags$img(src = "logo.png", style="height: 100px; margin-right: 20px; margin-bottom: 20px; float:left;"),
          HTML("This application provides an interactive user interface for the R
          package <a href='https://jeffsocal.github.io/tidyproteomics/' target='_blank' rel='noopener noreferrer' style='font-weight:600;'>tidyproteomics</a>. Users may upload their protein- or
          peptide-level data for abundance subsetting, contaminant removal, abundance
          normalization, differential expression analysis, and ontology
          enrichment.")
        ),
        br(),
        tags$div(
          style = "font-weight:600; text-align: justify; display: inline-block",
          "Please proceed to the 'Data Input and Summary' tab to begin your
          analysis. The analysis options available for your dataset will
          populate the sidebar after you have uploaded your data."
        )
      ),
      
      box(
        title = "Application features",
        status = "info",
        width = 10,
        HTML("<div>A detailed tutorial on using this application is available on its <a href='https://ejmackrell.github.io/tidyproteomics-interactive/' target='_blank'>dedicated documentation webpage.</a></div><br>"),
        timelineBlock(
          width = 12,
          
          timelineLabel("Data input and information"),
          timelineItem(
            title = "Application introduction",
            icon = icon("info"),
            color = "info",
            "The current page describes all the features contained in the application."
          ),
          timelineItem(
            title = "Data input and summary",
            icon = icon("file-arrow-up"),
            color = "info",
            "Users may upload an Excel file containing peptide- or
            protein-level data outputs from Proteome Discoverer and MaxQuant
            searches. After the file is processed into a tidyproteomics
            object, the user may view its attributes, such as annotation
            information, sample identifiers, and raw protein abundances."
          ),
          
          timelineLabel("Data preprocessing"),
          timelineItem(
            title = "Data subsetting and summary",
            icon = icon("filter"),
            color = "info",
            "Users may subset their data by any of the qualitative or
            quantitative variables present across the experiments, accounting,
            or annotation attributes of the data set. Users may also specify a
            text pattern for removing contaminant proteins from their data.
            Finally, sample groups can also be reassigned or renamed using the
            interactive spreadsheet."
          ),
          timelineItem(
            title = "Abundance normalization",
            icon = icon("scale-balanced"),
            color = "info",
            "Once data are uploaded and optionally subsetted, users can
            specify a method for imputing missing protein abundance values.
            Imputation can be specified to occur before or after protein
            abundance normalization, which can be performed using the methods
            chosen by the user in this tab."
          ),
          timelineItem(
            title = "Peptide collapse",
            icon = icon("minimize"),
            color = "info",
            HTML("This tab appears once peptide-level data are uploaded to the
            application. Peptide abundances are transformed into protein
            abundances by using the <i>collapse</i> function of
            <i>tidyproteomics</i>. <b>Please note that the collapse of
            normalized peptide abundances into protein-level data is not
            currently supported. </b>The unprocessed dataset will be used if the
            data are not modified in the data subsetting tab.")
          ),
          
          timelineLabel("Data analysis"),
          timelineItem(
            title = "Expression analysis",
            icon = icon("volcano"),
            color = "info",
            "Users may conduct differential expression analysis using the
            preprocessed protein abundance values by specifying sample groups
            for comparison and a statistical method for estimating fold
            changes and p-values. Results are displayed in an interactive
            volcano or proportional plot and an accompanying table."
          ),
          
          timelineItem(
            title = "Enrichment analysis",
            icon = icon("project-diagram"),
            color = "info",
            HTML("This tab appears after conducting differential expression analysis. Users may select a 
            group comparison and examine the differential expression data for enrichment
            of terms from the user-specified ontology. Users can specify either the <i>GSEA</i>
            algorithm or a <i>Wilcoxon rank sum</i> comparison for conducting this analysis.") 
          )
        ),
        div(style="font-weight:600;",
          "Copyright © 2023, California Institute of Technology (Caltech),
          based on support from the Institute for Collaborative
          Biotechnologies through cooperative agreement W911NF-19-2-0026 from
          the U.S. Army Research Office.  The content of the information does
          not necessarily reflect the position or the policy of the
          Government, and no official endorsement should be inferred.",
          br(),
          br(),
          "All rights reserved."
        )
        
      )
      
    )
  )
  
}

tab_introduction_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
  })
  
}