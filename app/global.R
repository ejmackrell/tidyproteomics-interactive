
library(shiny)
library(bs4Dash)
library(fresh)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinyFeedback)
library(tippy)
library(rhandsontable)

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
source("modules/tab_collapse.R")
source("modules/tab_expression_analysis.R")
source("modules/tab_enrichment_analysis.R")
source("modules/tab_introduction.R")

# Options for shinycssloaders, file upload limit, cli progress rendering
options(
  spinner.size = 0.75,
  spinner.color = "#5e8cbe7a",
  shiny.maxRequestSize = 2000 * 1024^2,
  cli.progress_show_after = 0
)

# Make cli progress human readable in Shiny notifications
Sys.setenv(NO_COLOR = 1)
  

# Overwrite default behavior of renaming uploaded file
rename_uploaded_file <- function(x) {
  
  old <- x$datapath
  new <- file.path(dirname(old), x$name)
  file.rename(old, new)
  
  x$datapath <- new
  x
  
}


# Column rendering definitions for summary table
build_summary_col_defs <- function(x) {
  
  col_def <- list()
  
  if ("CVs" %in% names(x)) {
    col_def <- col_def %>% 
      append(
        list(
          CVs = colDef(
            cell = JS("function(cellInfo, state) {
                          if (cellInfo.value != null) {
                            return cellInfo.value.toFixed(3)
                          } else {
                            return cellInfo.value
                          }
                        }"
            )
          )
        )
    )
  }
  
  if ("import_file" %in% names(x)) {
    
    col_def <- col_def %>% 
      append(
        list(
          import_file = colDef(
            minWidth = 400
          )
        )
      )
  }
  
  return(col_def)
  
}


# Available imputation methods
imputation_methods <- list(
  "min" = base::min,
  "median" = stats::median,
  "randomforest" = "randomforest"
)


# Available statistical methods for expression comparison
statistical_methods <- list(
  "t-test" = stats::t.test,
  "Wilcoxon test" = stats::wilcox.test,
  "Kolmogorov-Smirnov test" = stats::ks.test,
  "limma" = "limma"
)


# JS rendering of expression table
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


# JS rendering of enrichment table
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


# Comparison operators given subsetting data variable type
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

# Data features appropriate for display in table
tabular_data_features <- c(
  "experiments",
  "quantitative",
  "accounting",
  "annotations"
)


# Modified from https://github.com/jeffsocal/tidyproteomics/blob/main/R/plot_pca.R
# Modify PCA plot function from tidyproteomics for Plotly glyph compatibility
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


# Modified heatmap function to prevent file writing
# Required for successful plotting on Shiny Server
plot_heatmap_mod <- function(
    data = NULL,
  tag=NULL,
  row_names=FALSE,
  ...
){
  
  # visible bindings
  color <- NULL
  abundance <- NULL
  sample_rep <- NULL
  
  check_data(data)
  
  quantval <- data$quantitative_source
  data_quant <- data %>% extract(values = quantval, na.rm = TRUE) %>%
    dplyr::select(!dplyr::matches('^origin$'))
  theme_palette <- theme_palette()
  
  data_col <- data_quant %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(
      n = replicate %>% unique() %>% length(),
      .groups = 'drop'
    )
  
  data_col <- data_col %>%
    dplyr::mutate(color = theme_palette[1:nrow(data_col)])
  
  group_color <- list(sample = data_col %>% dplyr::select(color) %>% unlist() %>% as.vector())
  names(group_color$sample) <- data_col$sample
  
  data_munge <- data_quant %>%
    dplyr::mutate(abundance = log10(abundance)) %>%
    tidyr::pivot_wider(
      names_from = c('sample', 'replicate'),
      values_from = 'abundance',
      values_fill = 0
    ) %>% as.data.frame() %>%
    tibble::column_to_rownames('identifier') %>%
    as.matrix()
  
  col <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(32) %>% rev()
  
  plot <- pheatmap::pheatmap(data_munge,
    scale = "none",
    silent = TRUE,
    col =  col,
    show_rownames = row_names,
    annotation_colors = group_color,
    #annotation_legend = FALSE,
    #legend = FALSE,
    annotation_names_col = FALSE,
    annotation_col = data_quant %>%
      dplyr::select(sample, replicate) %>%
      unique() %>%
      tidyr::unite(sample_rep, sample, replicate, remove = F) %>%
      dplyr::select(!replicate) %>%
      as.data.frame() %>%
      tibble::column_to_rownames('sample_rep'),
    main = glue::glue("Heatmap: {data$analyte} {data$quantitative_source} \n")
  )
  
  pdf(NULL)
  
  if(!'null device' %in% names(grDevices::dev.cur())) { invisible(grDevices::dev.off()) }
  
  return(plot_save(plot,
    data,
    glue::glue("{data$analyte}_{data$quantitative_source}_heatmap"),
    ...))
  
}

environment(plot_heatmap_mod) <- asNamespace("tidyproteomics")



plot_variation_cv_mod <- function (data = NULL, ...) 
{
  abundance <- NULL
  origin <- NULL
  identifier <- NULL
  abundance_sd <- NULL
  abundance_mean <- NULL
  abundance_cv <- NULL
  cv_mean <- NULL
  abundance_95h <- NULL
  abundance_95l <- NULL
  dynamic_range <- NULL
  check_data(data)
  analyte <- data$analyte
  quant_values <- get_quant_names(data)
  data_quant <- data %>% extract(values = quant_values)
  quant_values_all <- c("raw", "median", "scaled", "linear", 
    "limma", "loess", "svm", "randomforest")
  norm_vals <- intersect(quant_values_all, quant_values)
  if (length(norm_vals) == 1) {
    cli::cli_alert_warning("Normalization has not yet been performed.")
    return(data)
  }
  data_quant_norm_range <- data_quant %>% dplyr::mutate(abundance = abundance %>% 
      log10(), origin = origin %>% as.factor()) %>% dplyr::filter(!is.infinite(abundance)) %>% 
    dplyr::group_by(origin, sample) %>% dplyr::summarise(abundance_min = min(abundance, 
      na.rm = T), abundance_max = max(abundance, na.rm = T), 
      abundance_95l = stats::quantile(abundance, 0.025, na.rm = T), 
      abundance_95h = stats::quantile(abundance, 0.975, na.rm = T), 
      dynamic_range = abundance_95h - abundance_95l, .groups = "drop") %>% 
    dplyr::ungroup()
  data_quant_cvs <- data_quant %>% dplyr::mutate(origin = sub("data_vals_", 
    "", origin), origin = sub("\\.rds", "", origin), origin = origin %>% 
      as.factor()) %>% dplyr::group_by(origin, identifier, 
        sample) %>% dplyr::summarise(abundance_mean = mean(abundance, 
          na.rm = T), abundance_sd = stats::sd(abundance, na.rm = T), 
          abundance_cv = abundance_sd/abundance_mean, .groups = "drop") %>% 
    dplyr::ungroup() %>% dplyr::filter(!is.na(abundance_cv))
  data_quant_cvs_mean <- data_quant_cvs %>% dplyr::group_by(origin, 
    sample) %>% dplyr::summarise(cv_mean = mean(abundance_cv, 
      na.rm = T), cv_sd = stats::sd(abundance_cv, na.rm = T), 
      .groups = "drop")
  n_colors <- data_quant_cvs_mean$sample %>% unique() %>% length()
  p_cv <- data_quant_cvs_mean %>% dplyr::mutate(origin = forcats::fct_relevel(origin, 
    norm_vals)) %>% ggplot2::ggplot(ggplot2::aes(origin, 
      cv_mean, color = sample)) + ggplot2::geom_point() + ggplot2::geom_line(ggplot2::aes(group = sample)) + 
    ggplot2::geom_hline(yintercept = 0, color = NA) + ggplot2::scale_y_continuous(n.breaks = 7) + 
    ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, 
      hjust = 1), legend.position = "none", axis.title.y = ggplot2::element_text(size = 9)) + 
    ggplot2::scale_color_manual(values = theme_palette(n_colors)) + 
    ggplot2::labs(subtitle = "Quantitative Variation") + 
    ggplot2::xlab("") + ggplot2::ylab("CVs (sd/mean)")
  p_dr <- data_quant_norm_range %>% dplyr::mutate(origin = forcats::fct_relevel(origin, 
    norm_vals)) %>% ggplot2::ggplot(ggplot2::aes(origin, 
      dynamic_range, color = sample)) + ggplot2::geom_point() + 
    ggplot2::geom_line(ggplot2::aes(group = sample)) + ggplot2::scale_y_continuous(n.breaks = 11) + 
    ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, 
      hjust = 1), axis.title.y = ggplot2::element_text(size = 9)) + 
    ggplot2::scale_color_manual(values = theme_palette(n_colors)) + 
    ggplot2::labs(subtitle = "Quantitative Dynamic Range") + 
    ggplot2::xlab("") + ggplot2::ylab("Dynamic Range (95%CI Log10)")
  
  plot <- data_quant_cvs_mean %>%
    dplyr::select(origin, sample, mean = cv_mean) %>%
    dplyr::mutate(stat = 'CVs (sd/mean)') %>%
    dplyr::bind_rows(
      data_quant_norm_range %>%
        dplyr::mutate(mean = abundance_95h - abundance_95l) %>%
        dplyr::select(origin, sample, mean) %>%
        dplyr::mutate(stat = 'Dynamic Range (95%CI Log10)')
    ) %>%
    dplyr::mutate(origin = forcats::fct_relevel(origin, norm_vals)) %>%
    ggplot2::ggplot(ggplot2::aes(origin, mean, color=sample)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group=sample)) +
    ggplot2::geom_hline(yintercept = 0, color=NA) +
    ggplot2::scale_y_continuous(n.breaks = 11) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~stat, scales='free', strip.position = 'left') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=30, hjust=1)) +
    ggplot2::scale_color_manual(values = theme_palette()) +
    ggplot2::labs(title = "Normalization",
                  subtitle = "Effect on Variation and Range",
                  x = '', y ='')
  
  return(plot_save(plot,
    data,
    glue::glue("{data$analyte}_normalizated_variation"),
    ...))
  
}

environment(plot_variation_cv_mod) <- asNamespace("tidyproteomics")
