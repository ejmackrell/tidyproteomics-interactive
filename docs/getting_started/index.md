---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: default
title: Getting started
nav_order: 2
---
### Remote access via Caltech PEL 
\\
This application is hosted for public use by the Proteome Exploration Laboratory at the Beckman Institute at Caltech:

[bioinformatics.pel.caltech.edu/tidyproteomics/](http://bioinformatics.pel.caltech.edu/tidyproteomics/)

You may also run the application locally by following the instructions below.

------------------------------------------------------------------------

### Local installation with Docker
\\
The application is available as a Docker image on [Docker Hub](https://hub.docker.com/r/ejmackrell/tidyproteomics-interactive). To use the application locally, follow these instructions:

1.  Clone this repository to a local directory using

    ``` bash
    $ git clone https://github.com/ejmackrell/tidyproteomics-interactive <dir>
    ```

2.  Download, install, and start [Docker Desktop](https://www.docker.com/)

3.  Navigate to the directory and run the command

    ``` bash
    $ docker compose up
    ```

    to pull the Docker image, build a Docker container, and run the application.

You may access the running application at [`http://localhost:3838`](http://localhost:3838). To change the port from the default 3838, edit the specification for `ports` in `docker-compose.yml` (e.g., to `5000:3838` for accessing the application at port 5000).

------------------------------------------------------------------------

### Local installation with RStudio
\\
You can also run and modify the application in RStudio by following the instructions below.

#### Cloning the repository in RStudio

1. Install [`{renv}`](https://github.com/rstudio/renv) (if it is not installed already) by executing
   
   ```
   install.packages("renv")
   ```
   in the R console.
   
3. Create a new version-controlled project in RStudio and provide the URL for this repository.

#### Restoring the project

1. Once the project is opened in your RStudio session, run the command
   
    ``` R
    renv::activate()
    ```
    
    to load the project environment, which should download the appropriate version of [`{BiocManager}`](https://github.com/Bioconductor/BiocManager). 

3. Restore the project with the command
   
   ``` R
   renv::restore()
   ```
   to install all of the packages required for the project. If the repository for a Bioconductor package is not identified, you can manually install these packages individually by executing
   
   ``` R
   renv::install("bioc::{package name}")
   ```
   
   or instead as a group by specifying Bioconductor repositories with
   
   ``` R
   renv::restore(repos = BiocManager::repositories())
   ```