# tidyproteomics-interactive <img src="www/logo.png" style="margin-left: 20px" align="right" height="125"/>

[![](https://img.shields.io/badge/Article-10.1186%2Fs12859.023.05360.7-8A2BE2)](https://doi.org/10.1186/s12859-023-05360-7)

This repository contains a Shiny application for interactively working with the R package [`{tidyproteomics}`](https://github.com/jeffsocal/tidyproteomics). 

This application is hosted by the Proteome Exploration Laboratory at the Beckman Institute at Caltech:

[bioinformatics.pel.caltech.edu/tidyproteomics/](http://bioinformatics.pel.caltech.edu/tidyproteomics/)

## Cloning the repository
Install [`{renv}`](https://github.com/rstudio/renv) (if it is not installed already) by executing `install.packages("renv")`. Then, create a new version-controlled project in RStudio and provide the URL for this repository. 

## Restoring the project
Once the project is opened in your RStudio session, execute `renv::activate()` to load the project environment, which should download the appropriate version of [`{BiocManager}`](https://github.com/Bioconductor/BiocManager). Then, run `renv::restore()` to install all of the packages required for the project. If the appropriate repository for a Bioconductor package may not be found, you can manually install these packages by executing `renv::install("bioc::{package name}")`or instead by specifying Bioconductor repositories as an argument when restoring the project with `renv::restore(repos = BiocManager::repositories())`. 

## Citation
If you use this application for your research, please cite our article.
> Jones, J., MacKrell, E.J., Wang, TY. *et al*. Tidyproteomics: an open-source R package and data object for quantitative proteomics post analysis and visualization. *BMC Bioinformatics* **24**, 239 (2023).
