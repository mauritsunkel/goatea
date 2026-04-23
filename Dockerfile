## set Docker image
FROM rocker/shiny:4.5.0

## GitHub link image to repository
LABEL org.opencontainers.image.source=https://github.com/mauritsunkel/goatea

## install system libraries
RUN apt-get update -y && apt-get install -y libcurl4-openssl-dev libpng-dev libssl-dev make \
    libicu-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libxml2-dev zlib1g-dev \ 
	cmake libx11-dev libcairo2-dev libsqlite3-dev libglpk-dev && rm -rf /var/lib/apt/lists/*

## setup renv configuration
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.1.5")'

## HuggingFace: create non-root user
RUN useradd -m -u 1000 user
WORKDIR /home/user/app

## copy renv lockfile to WORKDIR
COPY renv.lock .
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'

## copy local app code to container WORKDIR
COPY --chown=user:user . .

## launch app 
ENV HOME=/home/user \
	R_USER=/home/user \
    SHINY_LOG_STDERR=1 \
    APPLICATION_LOGS_TO_STDOUT=true
ENV R_DEFAULT_DEVICE=CairoPNG
EXPOSE 7860
### as non-root user
USER user
### (Shiny default expose: 3838, HuggingFace default expose: 7860)
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=7860)"]

### docker run -p 80:7860 goatea (browser: localhost)