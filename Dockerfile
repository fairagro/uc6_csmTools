FROM rocker/r-ver:4.4

COPY ./ /uc6_csmTools

WORKDIR /uc6_csmTools

# workaround to keep scripts as is, only needed for docker
RUN apt-get update && apt-get -y install sudo

# Install system dependencies
RUN ./install_requirements.sh

# install DSSAT
RUN ./install_dssat.sh

# install R packages
RUN R -e 'install.packages(c("renv", "languageserver", "devtools"))' 
RUN R -e 'options(renv.download.override = utils::download.file)'
RUN R -e 'renv::restore()'

ENTRYPOINT ["Rscript", "inst/etl_pipeline_example.R"]