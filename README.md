# csmTools
ETL functions for semi-automated data integration into crop simulation modeling


## Problem description
Crop experiment datasets are generally published as collections of tables describing the experimental design, management events, and measured variables. These tables are linked to one another by identifier variables, as in a relational database (primary/foreign keys).
Many crop simulation model frameworks, such as DSSAT and APSIM, have standard annotation conventions (i.e., variable names, units, relationships) that can be mapped with automated workflows to a single data dictionary, the [ICASA dictionary](https://docs.google.com/spreadsheets/u/0/d/1MYx1ukUsCAM1pcixbVQSu49NU-LfXg-Dtt-ncLBzGAM/pub?output=html), in order to facilitate model intercomparison.
The ICASA dictionary covers an extensive range of crop experiment variables for a variety of cropping systems. It follows a category structure analogous to common data structures of crop experiment datasets. However, because the relationships between different tables can be defined in multiple ways, crop experiment datasets must be reshaped to the standard input format to become usable as model inputs. This is an essential preliminary step to variable mapping, that can be carried out using existing tools such as the [ARDN vMapper](https://data.agmip.org/ardn/tools/vmapper) (for unstructured dataset) or with pre-defined data maps.


## Tool purpose
csmTools aims to facilitate the ETL process for crop modelers by offering functions to identify and reshape datasets, map variables, perform simulations, and evaluate the quality of model inputs and outputs with graphical tools.


## Current version
Currently, available functions have been developed based on a prototype dataset [Seehausen Long-term Fertilization Experiment](https://doi.org/10.20387/bonares-3nqn-41vn) published on the [BonaRes Repository](https://www.bonares.de/research-data). T
The ETL process can be chiefly divided into four steps: (1) data identification and reshaping, (2) variable mapping, (3) data transformation into model input, and (4) simulation and visualization.

## Installation
The package uses [renv](https://rstudio.github.io/renv/) for package management. To install all required packages call 
```R
renv::restore()
```
from an R command prompt. Linux users may need to execute `install_requirements.sh` to install required apt packages. If you need to install DSSAT there is a `install_dssat.sh` to clone and build the package on Linux. Windows Users can just use DSSATs installer. 
> [!WARNING]
> Currently the pipeline does not work with the latest DSSAT Version 4.8.2.12 ...

To install the csmTools package exexute 
```R
remotes::install_local(".")
```
or 
```R
remotes::install_github("fairagro/uc6_csmTools")
``` 
in an R command prompt

If you use Visual Studio Code you can just load the Devcontainer to have an already set up development enviroment.

## Test script
A [test script](inst/etl_pipeline_example.R) allows to run the entire pipeline, from raw data to simulation output, on the example data.
```bash
Rscript inst/etl_pipeline_example.R 
```
You can also run the pipeline using Docker and the provided Dockerfile and later on copy files with `docker cp`.
```bash
docker build . -t uc6_image
docker run uc6_image --name uc6_container
docker start uc6_container
docker cp uc6_container:/uc6_csmTools/Rplots.pdf ./Rplots.pdf
```

### Data sources
All [raw data](inst/extdata/lte_seehausen/0_raw), [model inputs](inst/extdata/lte_seehausen/1_out), and [model outputs](inst/extdata/lte_seehausen/1_sim) can be found in the [external data folder](inst/extdata/lte_seehausen).
One exception is the weather data which can be downloaded from the [DWD Open Data Server](https://www.dwd.de/EN/ourservices/opendata/opendata.html) using dedicated functions that leverage the [rdwd package] (https://github.com/cran/rdwd). Additional data sources will be progressively added.
The soil profile data used in the test script is currently a DSSAT generic soil profile that comes with the DSSAT software. Functions to find, access, and format soil profile data from other sources will be progressively added to the package.
Similarly, the crop genotype data used are currently generic DSSAT cultivars. A set of functions to estimate cultivar-specific genetic parameters based on expert knowledge will be developed.

### Data identification and reshaping
The function uses metadata documented with the [BonaRes metadata schema for long-term field experiments](https://tools.bonares.de/ltfe/lte-details/430/) to identify the structure of the dataset and re-arrange it according to the ICASA standard. This type of domain-specific metadata is crucial to the purpose of the workflows as it provides specific information (e.g., spatial coverage, design structure, number of replicates...) that allows the machine to reconstruct the dataset and identify its components.  

### Variable mapping
Variable mapping involves renaming column headers, converting numeric variables, recoding categorical variables, as well as more complex operations such as the creation of new variables based on existing ones. Variable mapping is currently performed using pre-defined lookup tables ("maps") that are provided in the [internal data folder](data/). Complex mapping operations have not been implemented into these files yet and are instead performed in a data-specific manner using data-wrangling libraries.

### DSSAT Crop Simulation Model
Simulations are performed with the [DSSAT package](https://github.com/palderman/DSSAT) that allows running the DSSAT CSM executable from within an R session. For this to work, DSSAT must installed onto the local machine.
The program and all the associated documentation can be downloaded [here](https://dssat.net)
It is recommended to use the default installation path (C:/DSSAT48/) as using custom paths may cause issues with running DSSAT from within R.

