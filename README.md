# tidyproteomics-interactive

This repository contains a Shiny application for interactively working with the R package [`{tidyproteomics}`](https://github.com/jeffsocal/tidyproteomics).

This application is graciously hosted by the Proteome Exploration Laboratory at the Beckman Institute at Caltech:

bioinformatics.pel.caltech.edu/tidyproteomics/

## Cloning the repository
Install [`{renv}`](https://github.com/rstudio/renv) (if it is not installed already) by executing `install.packages("renv")`. Then, create a new version-controlled project in RStudio and provide the URL for this repository. 

## Restoring the project
Once the project is opened in your RStudio session, execute `renv::activate()` to load the project environment, which should download the appropriate version of [`{BiocManager}`](https://github.com/Bioconductor/BiocManager). Then, run `renv::restore()` to install all of the packages required for the project. If the appropriate repository for a Bioconductor package may not be found, you can manually install these packages by executing `renv::install("bioc::{package name}")`or instead by specifying Bioconductor repositories as an argument when restoring the project with `renv::restore(repos = BiocManager::repositories())`. 
