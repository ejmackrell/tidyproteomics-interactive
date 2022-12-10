
library(shiny)
library(bs4Dash)
library(fresh)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)

library(dplyr)
library(magrittr)
library(reactable)
library(plotly)
library(purrr)
library(glue)

library(tidyproteomics)

source("modules/tab_upload_data.R")
source("modules/tab_normalize_abundances.R")
source("modules/tab_expression_analysis.R")
source("modules/tab_enrichment_analysis.R")


imputation_methods <- list(
  "min" = base::min,
  "median" = stats::median,
  "mean" = base::mean,
  "max" = base::max,
  "sum" = base::sum
)

statistical_methods <- list(
  "t-test" = stats::t.test,
  "Wilcoxon test" = stats::wilcox.test,
  "Kolmogorov-Smirnov test" = stats::ks.test,
  "limma" = "limma"
)

render_expression_reactable <- JS("function(cellInfo, state) {
  
    const fixed_cols = ['imputed', 'log2_foldchange', 'foldchange', 'limma_t_statistic', 'limma_B_statistic']
    const exp_cols = ['average_expression', 'proportional_expression', 'p_value', 'adj_p_value']
    
    if (cellInfo.value != null) {
  
      if (fixed_cols.includes(cellInfo.column.id)) {
    
        return cellInfo.value.toFixed(3)
    
      } else if (exp_cols.includes(cellInfo.column.id)) {
    
        return cellInfo.value.toExponential(3)
    
      } else {
    
        return cellInfo.value
    
      }
  
    } else {
  
    return cellInfo.value
    
    }
  
  }"
)


render_enrichment_reactable <- JS("function(cellInfo, state) {
  
    const fixed_cols = ['enrichment', 'enrichment_normalized', 'log2err']
    const exp_cols = ['p_value', 'adj_p_value']
  
    if (cellInfo.value != null) {
  
      if (fixed_cols.includes(cellInfo.column.id)) {
    
        return cellInfo.value.toFixed(3)
    
      } else if (exp_cols.includes(cellInfo.column.id)) {
    
        return cellInfo.value.toExponential(3)
    
      } else {
    
        return cellInfo.value
    
      }
  
    } else {
      
      return cellInfo.value
    
    }
  
  }"
)