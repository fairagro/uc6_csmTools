#' ####
#' 
#' ####
#' 
#' @param path ###
#' @param repo ###
#' @param sheet ###
#' 
#' @return ###
#' 
#' @importFrom xml2 read_xml xml_text xml_find_all xml_find_first
#' @importFrom purrr map
#' @importFrom dplyr bind_rows distinct
#' @importFrom readxl read_excel
#' @importFrom readODS read_ods
#' 
#' @export
#'

read_metadata <- function(path, repo, sheet = NULL){
  
  if (grepl("http", path)) {
    metadata <- read_xml(path)
  } else {
    metadata <- path
  }  
  
  # Read metadata
  metadata <- read_xml(path)
  
  # Provenance metadata
  prov_nodes <- xml_find_all(metadata, "//gmd:CI_ResponsibleParty")
  
  prov <- map(prov_nodes, function(x){
    
    persons <- xml_text(xml_find_first(x, ".//gmd:individualName"))
    organisation <- xml_text(xml_find_first(x, ".//gmd:organisationName"))
    position <- xml_text(xml_find_first(x, ".//gmd:positionName"))
    street <- xml_text(xml_find_first(x, ".//gmd:deliveryPoint"))
    city <- xml_text(xml_find_first(x, ".//gmd:city"))
    postal_code <- xml_text(xml_find_first(x, ".//gmd:postalCode"))
    email <- xml_text(xml_find_first(x, ".//gmd:electronicMailAddress"))
    
    data.frame(persons, organisation, position, street, city, postal_code, email)
  })
  
  prov_out <- bind_rows(prov) %>% distinct()
  
  # Identification information
  ident <- xml_find_all(metadata, "//gmd:identificationInfo")
  
  ident_info <- map(ident, function(x){
    
    doi <- xml_text(xml_find_first(x, ".//gmd:MD_Identifier"))
    title <- xml_text(xml_find_first(x, ".//gmd:alternateTitle"))
    
    data.frame(doi = doi, title = title)
  })
  
  ident_info_out <- ident_info[[1]]
  
  # Spatial and temporal extent
  extent <- xml_find_all(metadata, "//gmd:EX_Extent")
  
  spat_cov <- map(extent, function(x){
    
    wb_lon <- xml_text(xml_find_first(x, ".//gmd:westBoundLongitude"))
    eb_lon <- xml_text(xml_find_first(x, ".//gmd:eastBoundLongitude"))
    sb_lat <- xml_text(xml_find_first(x, ".//gmd:southBoundLatitude"))
    nb_lat <- xml_text(xml_find_first(x, ".//gmd:northBoundLatitude"))
    
    data.frame(latitude = c(wb_lon, eb_lon), longitude = c(sb_lat, nb_lat))
  })
  
  spat_cov_out <- spat_cov[[1]] %>% mutate_all(as.numeric)
  
  
  temp_cov <- map(extent, function(x){
    
    start <- xml_text(xml_find_first(x, ".//gml:beginPosition"))
    end <- xml_text(xml_find_first(x, ".//gml:endPosition"))
    
    data.frame(start = start, end = end)
  })
  
  temp_cov_out <- temp_cov[[2]]
  
  # Variable key
  key <- switch(repo,
                "bnr" = {
                  # Find all nodes that match a given XPath expression
                  var_nodes <- xml_find_all(metadata, "//bnr:MD_Column")
                  # Extract the data for each variable as a named vector
                  vars <- map(var_nodes, function(x) {
                    tbl_name <- xml_text(xml_find_first(x, ".//bnr:tableName"))
                    name <- xml_text(xml_find_first(x, ".//bnr:name"))
                    descript <- xml_text(xml_find_first(x, ".//bnr:description"))
                    methods <- xml_text(xml_find_first(x, "//bnr:methods"))
                    unit <- xml_text(xml_find_first(x, ".//bnr:unit"))
                    data_type <- xml_text(xml_find_first(x, ".//bnr:dataType"))
                    nas <- xml_text(xml_find_first(x, "//bnr:missingValue"))
                    # Group into a list
                    list(tbl_name = tbl_name, name = name, description = descript, methods = methods, unit = unit, 
                         data_type = data_type, missing_vals = nas)
                  })
                  # Combine the list of named vectors into a data frame
                  key <- bind_rows(vars)
                },
                "dwd" = {
                  # Find all nodes that match a given XPath expression
                  var_nodes <- xml_find_all(metadata, "//MetElement")
                  # Extract the data for each variable as a named vector
                  vars <- map(var_nodes, function(x) {
                    name <- xml_text(xml_find_first(x, ".//ShortName"))
                    unit <- xml_text(xml_find_first(x, ".//UnitOfMeasurement"))
                    descript <- xml_text(xml_find_first(x, ".//Description"))
                    # Group into a list
                    list(name = name, description = descript, unit = unit)
                  })
                  # Combine the list of named vectors into a data frame
                  key <- bind_rows(vars)
                },
                c("hdv","zdp","opa","sradi","odja") %in% {
                  # Determine file format
                  ext <- gsub("\\.", "", substr(metadata, nchar(metadata)-4+1, nchar(metadata)))
                  # Read key
                  switch(ext,
                         "txt" = {
                           key <- read.delim(metadata, header = TRUE, sep = "\t", fill = TRUE)
                         },
                         "csv" = {
                           lines <- readLines(metadata, n = 1)
                           if (grepl(",", lines)) { 
                             key <- read.csv(metadata, header = TRUE, sep = ",", fill = TRUE)
                           } else if (grepl(";", lines)) {
                             key <- read.csv(metadata, header = TRUE, sep = ";", fill = TRUE)
                           }
                         },
                         "xlsx" = {
                           key <- read_excel(metadata, sheet = sheet)
                         },
                         "ods" = {
                           key <- read_ods(metadata, sheet = sheet)
                         },
                         {
                           print("Invalid file format") # add json/xml
                         }
                  )
                  
                }
  )
  
  
  # Output
  out <- list(Identification = ident_info_out,
              Provenance = prov_out,
              Spatial_coverage = spat_cov_out,
              Temporal_coverage = temp_cov_out,
              Variable_key = key)
  return(out)
  
}
