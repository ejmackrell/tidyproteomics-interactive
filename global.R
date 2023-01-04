
library(shiny)
library(bs4Dash)
library(fresh)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinyFeedback)
library(tippy)

library(dplyr)
library(magrittr)
library(reactable)
library(plotly)
library(purrr)
library(rlang)
library(glue)

library(tidyproteomics)

source("modules/tab_upload_data.R")
source("modules/tab_subset.R")
source("modules/tab_normalize_abundances.R")
source("modules/tab_expression_analysis.R")
source("modules/tab_enrichment_analysis.R")
source("modules/tab_introduction.R")

options(
  spinner.size = 0.75,
  spinner.color = "#5e8cbe7a",
  shiny.maxRequestSize = 30 * 1024^2,
  cli.progress_show_after = 0
)

Sys.setenv(NO_COLOR = 1)
  

rename_uploaded_file <- function(x) {
  
  old <- x$datapath
  new <- file.path(dirname(old), x$name)
  file.rename(old, new)
  
  x$datapath <- new
  x
  
}

imputation_methods <- list(
  "min" = base::min,
  "median" = stats::median,
  "randomforest" = "randomforest"
  # "mean" = base::mean,
  # "max" = base::max,
  # "sum" = base::sum
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

subsetting_operators <- list(
  "character" = list(
    "%like%",
    "! %like%",
    "==",
    "!="
  ),
  "logical" = list(
    "==",
    "!="
  ),
  "numeric" = list(
    "<",
    ">",
    "<=",
    ">=",
    "==",
    "!="
  )
)

tabular_data_features <- c(
  "experiments",
  "quantitative",
  "accounting",
  "annotations"
)


plot_pca_mod <- function (data = NULL, variables = c("PC1", "PC2"), labels = TRUE, 
  label_size = 3, ...) 
{
  identifier <- NULL
  abundance <- NULL
  var <- NULL
  princom <- NULL
  val <- NULL
  pcn <- NULL
  sample_exp <- NULL
  variables <- variables[1:min(length(variables), 2)]
  variables <- rlang::arg_match(variables, c("scree", paste0("PC", 
    1:9)), multiple = T)
  check_data(data)
  analyte <- data$analyte
  quantval <- data$quantitative_source
  data_quant <- data %>% extract(values = quantval, na.rm = TRUE)
  dt_pca <- data_quant %>% dplyr::mutate(sample = paste(sample, 
    replicate, sep = ".")) %>% dplyr::select(identifier, 
      sample, abundance) %>% tidyr::pivot_wider(names_from = "identifier", 
        values_from = "abundance", values_fill = 0) %>% tibble::column_to_rownames("sample") %>% 
    as.data.frame()
  dt_pca <- dt_pca[, which(apply(dt_pca, 2, var, na.rm = TRUE) != 
      0)]
  pca_res <- stats::prcomp(dt_pca, scale. = TRUE)
  pca_sum <- base::summary(pca_res)$importance %>% tibble::as_tibble() %>% 
    dplyr::mutate(var = c("stdev", "prop_var", "cum_var")) %>% 
    tidyr::pivot_longer(!var, names_to = "princom", values_to = "val") %>% 
    dplyr::mutate(pcn = sub("[A-Z]+", "", princom) %>% as.numeric())
  if (length(variables) == 1 && variables == "scree") {
    plot <- pca_sum %>% dplyr::mutate(val = val * 100) %>% 
      dplyr::filter(var == "prop_var") %>% ggplot2::ggplot(ggplot2::aes(pcn, 
        val)) + ggplot2::geom_bar(stat = "identity") + ggplot2::geom_text(ggplot2::aes(label = paste0(signif(val, 
          2), "%")), hjust = 0.5, vjust = -0.2) + ggplot2::scale_x_continuous(breaks = unique(pca_sum$pcn)) + 
      ggplot2::scale_y_continuous(limits = c(0, 100)) + 
      ggplot2::labs(x = "Principal Component", y = "Proportion of Total Variance", 
        title = "PCA Scree Plot", subtitle = "protein abundance log2 co-variance") + 
      ggplot2::theme_classic()
  }
  else {
    pcas <- pca_sum %>% dplyr::select(princom) %>% unique() %>% 
      unlist() %>% as.character()
    pcas <- intersect(pcas, variables[1:2])
    pcx <- pcas[1]
    pcy <- pcas[2]
    pcx_p <- pca_sum %>% dplyr::filter(princom == pcx & var == 
        "prop_var") %>% dplyr::select(val) %>% unlist() %>% 
      as.numeric()
    pcy_p <- pca_sum %>% dplyr::filter(princom == pcy & var == 
        "prop_var") %>% dplyr::select(val) %>% unlist() %>% 
      as.numeric()
    plot <- pca_res$x %>% as.data.frame() %>% dplyr::rename(pcx = dplyr::all_of(pcx), 
      pcy = dplyr::all_of(pcy)) %>% tibble::rownames_to_column("sample_exp") %>% 
      tidyr::separate(sample_exp, into = c("sample", "replicate"), 
        sep = "\\.", remove = F) %>% ggplot2::ggplot(ggplot2::aes(pcx, 
          pcy, text = sample_exp)) + ggplot2::geom_point(ggplot2::aes(color = sample), # Add sample_exp as text aesthetic mapping
            alpha = 0.5, size = 5)
    if (labels == TRUE) {
      plot <- plot + ggplot2::geom_text(ggplot2::aes(label = sample_exp), # Add supported text geom for ggplotly
        size = label_size)
    }
    plot <- plot + ggplot2::scale_color_manual(values = theme_palette()) + 
      ggplot2::labs(x = paste0(pcx, " (", signif(pcx_p * 
          100, 3), "%)"), y = paste0(pcy, " (", signif(pcy_p * 
              100, 3), "%)"), title = paste("PCA analysis", 
                pcx, "~", pcy), subtitle = paste0("protein abundance log2 co-variance (", 
                  quantval, ")")) + ggplot2::theme_classic() + 
      ggplot2::theme(axis.text = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_blank())
  }
  return(plot_save(plot, data, glue::glue("{data$analyte}_{data$quantitative_source}_pca_{paste(variables, collapse='')}"), 
    ...))
}

environment(plot_pca_mod) <- asNamespace("tidyproteomics")
