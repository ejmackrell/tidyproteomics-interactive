# syntax=docker/dockerfile:1

# Use Shiny image for R 4.2.2
FROM rocker/shiny:4.2.2

# Install Debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
# Copy application and lockfile
COPY renv.lock ./renv.lock
COPY /app ./app

# Install renv, BiocManager, and required packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::install("BiocManager@1.30.19")'
RUN Rscript -e 'options(timeout = 300); renv::restore()'

# Port metadata
EXPOSE 3838

# Launch application
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
