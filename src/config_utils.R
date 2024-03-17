# ============================================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# ============================================================================================
# @Goal: Create configuration file for setup of packages and functions used in the project
#
# @Date: fev 2024
# @author: Marcos

# Load libraries - groundhog increases code reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Loading required packages 
groundhog.library(
  pkg  = c(
    "assertthat", "broom", "cartography", "compiler", "data.table", "dplyr", "exactextractr",
    "foreign", "furrr", "ggplot2", "haven", "lubridate", "lwgeom", "stringr",
    "raster", "RColorBrewer", "readr", "readxl", "scpi", "sf", "sp",
    "terra", "viridis"),
  date = "2023-09-01"
  )

here::i_am(".gitignore")
# ############################################################################################
# Functions
# ############################################################################################

# Function --------------------------------------------------------------------
# @Arg       : file_path is vector with the full name of the excel files
# @Output    : Dataframe with 5 columns in a tidy data format
# @Purpose   : Read project issues, process it and create a tidy version by year
# @Desc      : Open excel files to collect information about issues amount, year of vintage,
#   year of crediting, year of retirement, and ID. Create a tidy dataframe containing year, ID,
#   and credits
# @Written_on: 11/12/2023
# @Written_by: Marcos Paulo
process_project_info <- function(file_path) {
  # Read data from Excel file
  project_data <- read_excel(file_path)
  
  # Substituting white spaces per underline
  colnames(project_data) <- gsub(" ", "_", colnames(project_data))
  
  # Filter dataset
  filtered_data <- project_data %>%
    dplyr::select(
      c(
        "Issuance_Date",
        "Vintage_Start",
        "ID",
        "Retirement/Cancellation_Date",
        "Quantity_Issued"
      )
    )
  # Get the VERRA ID
  id <- unique(filtered_data$ID)
  
  # Create separate data frames for each date type
  issuance_df <- filtered_data %>%
    mutate(year = year(Issuance_Date)) %>%
    group_by(year) %>%
    summarise(issued_credits = sum(Quantity_Issued)) %>%
    mutate(ID = id) # Get the Verra ID
  
  vintage_df <- filtered_data %>%
    mutate(year = year(Vintage_Start)) %>%
    group_by(year) %>%
    summarise(vintage_credits = sum(Quantity_Issued)) %>%
    mutate(ID = id) # Get the Verra ID
  
  retirement_df <- filtered_data %>%
    mutate(year = year(`Retirement/Cancellation_Date`)) %>%
    filter(!is.na(year)) %>%  # Removing NA values - happens in the year column
    group_by(year) %>%
    summarise(retired_credits = sum(Quantity_Issued)) %>%
    mutate(ID = id)
  
  # Set all data frames into list and then merge all data frames in list
  list_df <- list(issuance_df, vintage_df, retirement_df)
  final_df <- Reduce(function(x, y)
    merge(x, y, all = TRUE), list_df)
  
  # Return the final dataframe
  return(final_df)
}


# Function --------------------------------------------------------------------
# @Arg       : df is a dataframe with projects previous open 
# @Arg       : files_list is a list of Excel files to loop over
# @Output    : Dataframe with specific columns from 'df' modified
# @purpose   : Read project issues, save certifications and add info into 'df'
# @desc      : Open issues excel files to get information about the type of certifications a
#   project has. Then collect all unique certifications, and mutate the value of a column
#   representing that certification to 1 and 0 otherwise. The function goes over all Excel files
# @Written_on: 12/12/2023
# @Written_by: Marcos Paulo
add_certifications <- function(df, files_list) {
  # Loop over the list of files
  for (file in files_list) {
    # Read data from Excel file
    project_data <- read_excel(file)
    
    # Substituting white spaces per underline
    colnames(project_data) <- gsub(" ", "_", colnames(project_data))
    
    # Select only necessary columns
    project_data <- project_data %>%
      dplyr::select(c("ID", "Additional_Certifications"))
    
    # Replace NA values with "" so that the strsplit function run smoothly
    project_data[is.na(project_data)] <- ""
    
    # Get the Verra id
    id <- unique(project_data$ID)
    
    # Collect all unique values in "Additional_Certifications" column
    certifications <- strsplit(unique(project_data$Additional_Certifications),
                               split = ";")
    certifications <- str_trim(unlist(certifications)) # Unlist without spaces
    
    cat("Certification(s) of project ", as.character(id), ":",
        "\n", certifications, "\n", "\n")
    
    # Convert certifications and data columns to lowercase for comparison
    lower_certifications <- tolower(certifications)
    colnames(df) <- tolower(colnames(df))
    
    # Find rows with matching ID
    matching_row <- df[df$id_registry == as.character(id), ]
    
    # Update matching rows
    for (cert in lower_certifications) {
      if (cert %in% colnames(matching_row)) {
        matching_row[[cert]] <- 1
      }
    }
    # Replace updated rows in original data
    df[df$id_registry == as.character(id), ] <- matching_row
  } # End of the first loop over each Excel file
  
  # Return updated data
  return(df)
}


# Function --------------------------------------------------------------------
# @Arg       : list_years is a vector of years that raster files comprehends
# @Arg       : crop_extension is a spatial object (sf dataframe) with extension properties
# @Output    : No output, it saves cropped files as they are created
# @Purpose   : crop each raster file based on the them on crop_extension properties
# @Desc      : Open raster files from head(list_years, n = 1) to tail(list_years, n = 1), crop
#   each on based on crop_extension and save it on 'data/Land_coverage/"
# @Written_on: 10/03/2024
# @Written_by: Marcos Paulo
crop_raster_files <- function(list_years, crop_extension){
  # Loop through the years
  for (year in years) {
    # Construct the file name
    file_name <- paste0("brasil_coverage_", year, ".tif")
    cat("\n","Working with the following file: ", file_name, "\n", "\n")
    
    # Open the "tiff" file
    raster_data <- terra::rast(paste0(here::here("data", "Land_coverage"),  "/", file_name))
    
    # Mask the raster (created based on using the legal_amazon shapefile)
    raster_data_legal_amazon <- terra::crop(raster_data, terra::vect(crop_extension))
    
    # Save the new cropped ".tiff" file
    writeRaster(
      x         = raster_data_legal_amazon,
      filename  = paste0(here::here("data", "Land_coverage"),
                         "/brasil_coverage_legal_amazon_",
                         year,
                         ".tif"
      ),
      overwrite = TRUE
    )
  }
}


# Function --------------------------------------------------------------------
# @Arg       : list_state is a vector of states there is land tenure info on
# @Arg       : sf_dataframe is a spatial object (sf dataframe)
# @Arg       : tenure_geometry is a logical value to determine the geometry
# @Output    : Dataframe with merged data from sf_dataframe and data from land tenure
# @Purpose   : Collect information of all properties that have REDD projects inside
# @Desc      : Open each state land tenure dataset, collect all properties that have REDD+
#   projects within then, and merge project and land tenure datasets to know both project and
#   land tenure information.
# @Written_on: 10/03/2024
# @Written_by: Marcos Paulo
get_land_tenure_info <- function(list_state, sf_dataframe, tenure_geometry = TRUE){
  
  # Type checking for tenure geometry variable
  assertthat::assert_that(is.logical(tenure_geometry))

  # Create an empty data frame to store the data
  projects_land_tenure <-  data.frame()
  
  # Loop over the states where we have CAR information
  for (state in list_state){
    
    # Read the CAR file for each state and make it as SF object
    land_tenure <-
      sf::st_read(paste0(here::here("data", "Atlas_agriculture"), '\\', state)) %>%
      st_as_sf(wkt = "geom")
    
    # Add the correct CRS - https://atlasagropecuario.imaflora.org/downloads 
    st_crs(land_tenure) <- 4674 # Using this CRS based on their methodology
    
    # Make the SF polygons valid and convert CRS
    land_tenure <- sf::st_make_valid(land_tenure) %>%
      st_transform(crs = st_crs(sf_dataframe))  
    
    # Check if a project is within a property on state land tenure 
    intersections <- st_intersection(sf_dataframe, land_tenure) %>% 
      subset(!is.na(id))  # Remove lines with NA values in "id"
    
    # Replace the geometry in the merged result with the geometry from 'land_tenure'
    if (tenure_geometry) {
      intersections$geom <-
        st_geometry(land_tenure[match(intersections$id, land_tenure$id),])
    }
    
    # Add merged dataset to new dataframe
    projects_land_tenure <- rbind(projects_land_tenure, intersections)
    
  }
  # Return updated data
  return(projects_land_tenure)
}


# Function --------------------------------------------------------------------
# @Arg       : list_state is a vector of states there is land tenure info on
# @Arg       : sf_dataframe is a spatial object (sf dataframe)
# @Output    : Dataframe with all federal state conservation units the function was applied on
# @Purpose   : Collect information of all Conservation units in list_states states
# @Desc      : Open each state land tenure dataset, collect all Conservation Units from it and
#   then save them in a dataframe
# @Written_on: 10/03/2024
# @Written_by: Marcos Paulo
get_conservation_units <- function(list_state, sf_dataframe){
  # Create an empty data frame to store the data
  conservation_units <-  data.frame()
  
  # Loop over the states where we have land tenure information
  for (state in list_state){
    
    # Read the land tenure file for each state and make it as SF object
    land_tenure <-
      sf::st_read(paste0(here::here("data", "Atlas_agriculture"), "\\", state)) %>%
      st_as_sf(wkt = "geom")
    
    # Add the correct CRS - https://atlasagropecuario.imaflora.org/downloads 
    st_crs(land_tenure) <- 4674 # Using this CRS based on their methodology
    
    # Make the SF polygons valid and convert CRS
    land_tenure <- sf::st_make_valid(land_tenure) %>%
      st_transform(crs = st_crs(sf_dataframe))  
    
    # Filter land tenure data for conservation units
    land_tenure_filtered <- land_tenure %>% 
      filter(desc_subclass == "Unidade de Conservação de Uso Sustentável" | 
             desc_subclass == "Unidade de Conservação de Proteção Integral")

    # Add merged dataset to new dataframe
    conservation_units <- rbind(conservation_units, land_tenure_filtered)
    
  }
  # Return updated data
  return(conservation_units)
}


# Function --------------------------------------------------------------------
# @written_on: 12/01/2024
# @written_by: Marcos Paulo
# @Arg       : list_of_rast_files is a List of raster objects (their long name) 
# @Arg       : sf_dataframe is a sf dataframe containing either land tenure or Projects data
# @Output    : Dataframe with +- 40 columns with coverage land use, id and year
# @purpose   : Get the coverage land use of properties in IMAFLORA dataset
# @desc      : The function works in following way:
# - for each raster file variable (one for each year) - i
#   - for each property in the land tenure dataset - j
#       - Do processing to get all the coverage usage of the properties in a 
#       - new dataset, saving the ID of the property and the year of land use
get_coverage_properties <- function(list_of_rast_files, sf_dataframe) {
  
  # Get time of execution
  start <- Sys.time()
  
  # Create an empty data frame to store the data
  coverage_car <-  data.frame()
  
  # Open all raster files as a list
  raster_list  <- lapply(raster_files, raster)
  
  # Init the first loop
  for (i in seq_along(raster_list)) {

    cat(paste0( "Working on the following raster:", 
                "\n",
                basename(list_of_rast_files[i]),
                "\n"))
    
    # Get the year of the raster
    raster_name <- filename(raster_list[[i]])
    year_string <- substr(raster_name,
                          nchar(raster_name) - 7,
                          nchar(raster_name) - 4)
    year_num <- as.numeric(year_string)

    # Init the second loop 
    for (j in 1:nrow(sf_dataframe)) {
      # Take the j line of the land data
      property <- sf_dataframe[j,]
      
      # Extract from raster only the important spatial area
      extraction <- exact_extract(raster_list[[i]], property)
      
      # Create a data frame, sum the total area of each legend class, invert
      # lines with columns, add ID and YEAR columns to it 
      raster_property <- extraction[[1]] %>%
        group_by(value) %>% 
        summarise(class_id = sum(coverage_fraction)) %>%
        tidyr::pivot_wider(names_from = value, values_from = class_id) %>% 
        mutate(id = property$id,
               year = year_num)
      
      # Add values to dataframe
      coverage_car <- bind_rows(coverage_car, raster_property)
    }
  }
  
  # Show time of code execution
  print( Sys.time() - start )
  
  # Return the final dataframe
  return(coverage_car)
}
