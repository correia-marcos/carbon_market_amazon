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
    "foreign", "furrr", "ggplot2", "haven", "lubridate", "lwgeom", "patchwork", 
    "purrr", "pdftools", "stringr", "raster", "RColorBrewer", "readr", "readxl",
    "rnaturalearth", "rnaturalearthdata", "scpi", "scales", "sf", "sp", "terra",
    "tidyr", "viridis"),
  date = "2024-03-03")

here::i_am(".gitignore")


# ############################################################################################
# Functions
# ############################################################################################
# Function --------------------------------------------------------------------
# @Arg       : start_year is number representing first year to download
# @Arg       : end_year is number representing last year to download
# @Output    : no output
# @Purpose   : Function downloads all required raster files
# @Written_on: 10/01/2024
# @Written_by: Marcos Paulo
download_mapbiomas <- function(start_year = 2000, end_year = 2022) {
  base_url <- "https://storage.googleapis.com/mapbiomas-public"
  initiative <- "/initiatives/brasil"
  collection <- "/collection_8/lclu/coverage"
  
  for(year in start_year:end_year) {
    file_name <- paste0("/brasil_coverage_", year, ".tif")
    full_url <- paste0(base_url, initiative, collection, file_name)
    
    # Directory for download
    save_path <- paste0(here::here("data", "Land_coverage"), "/brasil_coverage_", year, ".tif")
    
    # Make the download
    download.file(full_url, save_path, mode="wb")
    
    cat("Downloaded: ", save_path, "\n")
  }
}



# Function --------------------------------------------------------------------
# @Arg       : "ets_df" is dataframe containing information about worldwide ETS 
# @Arg       : vector_states is a vector containing the US states or countries in EU
# @Arg       : ets_name is a string with the name "RGGI" or "EU-ETS" as it is written on ets_df
# @Output    : Dataframe with new lines consisting of states of RGGI
# @Purpose   : Replicate the ets_name row for each state/country in the vector_states
# @Desc      : Replicate the ets_name row for each state/country in the vector_states, since
# the ets_df was not in a tidy format. Sam
# @Written_on: 11/04/2024
# @Written_by: Marcos Paulo
expand_ets <- function(ets_df, vector_states, ets_name = "RGGI") {
  # Find the row with 'RGGI' in 'Jurisdiction covered'
  rggi_row <- ets_df[ets_df$`Jurisdiction covered` == ets_name, ]
  
  # Stop execution and show error if no 'RGGI' entry is found
  if(nrow(rggi_row) == 0) {
    stop("No 'ets_name' entry found in the 'Jurisdiction covered' column.")
  }
  
  # Replicate the 'RGGI' row for each state in the 'rggi' vector
  rggi_rows <- do.call(rbind, replicate(length(vector_states), rggi_row, simplify = FALSE))
  rownames(rggi_rows) <- NULL  # Clear row names
  rggi_rows$`Jurisdiction covered` <- vector_states  # Replace 'RGGI' with each state's name
  
  # Remove the original 'RGGI' row and add the new rows
  ets_df <- ets_df[ets_df$`Jurisdiction covered` != ets_name, ]
  ets_df <- rbind(ets_df, rggi_rows)
  
  return(ets_df)
}


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


# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : legend_df is a dataframe with information about the meaning of raster values
# @Arg       : column_id is the name of a column where value of the raster can be found
# @Arg       : column_description is the name of a column with the description of values
# @Arg       : land_coverage_df is a dataframe composed of columns with the id as column name
# @Output    : Arranged Dataframe from land_coverage_df with numerical column names changed
# @purpose   : Change the column names to make it easier to process
# @desc      : Map the association between column_id and column_description and changed column
# names according to its value. It also arrange the dataframe in panel (tidy) format and change
# order of two columns in the Land_coverage_df
convert_columns_panel <- function(legend_df, column_id, column_description, land_coverage_df){
  
  # Make sure the required column have the correct class
  legend_df <- legend_df %>% 
    mutate_at(vars(column_id, column_description), as.character)

  # Get a vector corresponding to the column args
  id_vector <- c(legend_df[[column_id]])
  description_vector <- c(legend_df[[column_description]])
  
  # Change column names in 'land_coverage_df' according to a map between both columns
  setnames(land_coverage_df, id_vector, description_vector, skip_absent=TRUE)
  
  # Arrange and reorder columns
  if (all(c("id", "year") %in% names(land_coverage_df))) {
    land_coverage_df <- land_coverage_df %>% 
      dplyr::arrange(id, year) %>% 
      dplyr::select(any_of(c("id", "year")), everything())
  }
  
  if (all(c("id_registry", "year") %in% names(land_coverage_df))) {
    land_coverage_df <- land_coverage_df %>%
      dplyr::arrange(id_registry, year) %>%
      dplyr::select(any_of(c("id_registry", "year")), everything())
  }
  
  # Return the dataframe with changed column names and arranged in panel format
  return(land_coverage_df)
}


# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : land_coverage_df is a dataframe composed with land use coverage data
# @Arg       : property_project_df is a dataframe with info about REDD+ projects and properties
# @Output    : Arranged Dataframe from land_coverage_df with summarized values by id_registry
# @purpose   : Aggregated values from the same project when there exits many values
# @desc      : The function first created a column "id_registry" if it doesn't exist. Then it
# summarized values by year and id_registry so that we can reduce dataframe size
agg_coverage_values <- function(land_coverage_df, property_project_df){
  
  # Create id_registry column if the dataframe don't already contain it
  if ("id" %in% names(land_coverage_df)) {
  land_coverage_df <- land_coverage_df %>%
    mutate(id_registry = as.character(
      property_project_df$id_registry[match(land_coverage_df$id, property_project_df$id)]))
  }
  
  # Convert all NA values to 0
  land_coverage_df <- land_coverage_df %>%
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  # Group the values by "year" / "id_registry" and sum the values in numerical columns
  # Check if column "id" exists
  if("id" %in% names(land_coverage_df) && is.character(land_coverage_df$id)) {
    # Column "id" exists and is type character - remove it and proceeds
    summarized_df <- land_coverage_df %>%
      dplyr::select(!id) %>% 
      group_by(id_registry, year) %>%
      summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
      dplyr::arrange(id_registry, year) %>%
      dplyr::select(any_of(c("id_registry", "year")), everything())
  } else {
    # Column id doesn't exist or is not character type
    summarized_df <- land_coverage_df %>%
      group_by(id_registry, year) %>%
      summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
      dplyr::arrange(id_registry, year) %>%
      dplyr::select(any_of(c("id_registry", "year")), everything())  
  }
  
  # Return aggregated dataframe
  return (summarized_df)
}


# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : land_coverage_df is a dataframe composed with land use coverage data
# @Arg       : legend_df is a dataframe with information about the meaning of raster values
# @Arg       : projects_df is a sf dataframe containing information about REDD+ projects
# @Output    : Dataframe with reduced number of columns - aggregated values
# @purpose   : Aggregate values of land use that represent the same general Class of data
# @desc      : The function first delimits in legend_df which values corresponds to each
# general class in MAPBIOMAS data. It Then create new columns based on this legend definition.
# The function also merge values with projects_df datasets
# Please check https://brasil.mapbiomas.org/codigos-de-legenda/ - legend 8
get_coverage_classes <- function(land_coverage_df, legend_df, projects_df){
  
  # Get the classes corresponding with Forest and select only the needed classes
  Forest <- as.character(c(legend_df[1:6, "Description"])[[1]])
  Forest <- Forest[Forest %in% names(land_coverage_df)]
  # Get the classes corresponding with Non Forest Natural Formation and select only the needed
  Non_Forest_Natural <- as.character(c(legend_df[7:13, "Description"])[[1]])
  Non_Forest_Natural <- Non_Forest_Natural[Non_Forest_Natural %in% names(land_coverage_df)]
  # Get the classes corresponding with Farming and select only the needed classes
  Farming <- as.character(c(legend_df[15:29, "Description"])[[1]])
  Farming <- Farming[Farming %in% names(land_coverage_df)]
  # Get the classes corresponding with Non vegetated area and select only the needed classes
  Non_vegetated_area <- as.character(c(legend_df[30:34, "Description"])[[1]])
  Non_vegetated_area <- Non_vegetated_area[Non_vegetated_area %in% names(land_coverage_df)]
  # Get the classes corresponding with Water and select only the needed classes
  Water <- as.character(c(legend_df[35:37, "Description"])[[1]])
  Water <- Water[Water %in% names(land_coverage_df)]
  
  # Create new dataframe based by aggregating different columns
  covered_agg <- land_coverage_df %>% 
    mutate(Forest             = rowSums(land_coverage_df[Forest], na.rm = TRUE),
           Non_Forest_Natural = rowSums(land_coverage_df[Non_Forest_Natural], na.rm = TRUE),
           Farming            = rowSums(land_coverage_df[Farming], na.rm = TRUE),
           Non_vegetated_area = rowSums(land_coverage_df[Non_vegetated_area], na.rm = TRUE),
           Water              = rowSums(land_coverage_df[Water], na.rm = TRUE))
  
  if ("id_registry" %in% names(land_coverage_df)){
    covered_agg <- covered_agg %>%
      # Select only the new columns plus identifiable columns
      dplyr::select(id_registry, year, Forest, Non_Forest_Natural, Farming, Non_vegetated_area,
        Water, Not_Observed)
  }else {
    covered_agg <- covered_agg %>%
      # Select only the new columns plus identifiable columns
      dplyr::select(id, year, Forest, Non_Forest_Natural, Farming, Non_vegetated_area, Water,
                    Not_Observed)
  }
  # Return new created dataframe
  return(covered_agg)
}

# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : land_coverage_df is a dataframe composed with land use coverage data
# @Output    : Dataframe with percentage value instead of raw value
# @purpose   : Calculate percentage of each class type in level Mapbiomas land use class
# @desc      : The function simply transform raw value to percentages
calculate_land_percentages <- function(land_coverage_df){
  land_coverage_df <- land_coverage_df %>%
    # Step 1: Calculate the total sum of the columns of interest for each row
    mutate(Total = rowSums(dplyr::select(., Forest, Non_Forest_Natural, Farming,
                                         Non_vegetated_area, Water, Not_Observed),
                           na.rm = TRUE)) %>%
    # Step 2: Calculate the percentages for each column
    mutate(across(c(Forest, Non_Forest_Natural, Farming, Non_vegetated_area,
                    Water, Not_Observed), 
                  ~ .x / Total * 100, .names = "{.col}_pctg")) %>%
    # Remove unnecessary column
    dplyr::select(-Total, -Forest, -Non_Forest_Natural, -Farming, -Non_vegetated_area,
                  -Water, -Not_Observed)
  
  return(land_coverage_df)
}

# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : land_coverage_df is a dataframe composed with land use coverage data
# @Arg       : projects_df is a dataframe containing information about REDD+ projects
# @Arg       : column_by is a string containing the name of a column to use as the by column
# @Output    : Dataframe with merged information
# @purpose   : Merge dataframes
# @desc      : The function simply merge two datasets
merge_data_synthetic <- function(land_coverage_df, information_df, column_by){
  
  # Convert id_registry into the correct type - for merging
  if ("id_registry" %in% names(information_df)){
    information_df <- information_df %>% 
      mutate(id_registry = as.double(id_registry))
  }
  
  # Merge datasets and create new columns
  land_coverage_df <- land_coverage_df %>% 
    left_join(information_df, by = column_by) %>%
    mutate(Total_natural_formation = Forest_pctg + Non_Forest_Natural_pctg,
           .after = year) %>% 
    dplyr::select(-geom)
  
  return(land_coverage_df)
}


# Function --------------------------------------------------------------------
# @written_on: 12/02/2024
# @written_by: Marcos Paulo
# @Arg       : land_coverage_df is a dataframe composed with land use coverage data
# @Arg       : final_year is a number of the last year to take into account
# @Arg       : criterion is a number corresponding to the percentage of tolerance 
# @Output    : Dataframe with filtered information
# @purpose   : Function to filter Conservation Units without significant deforestation
# @desc      : The function simply create a variation column and filter values in it

filter_no_deforestation <- function(land_coverage_df, final_year, criterion) {
  # Ensuring 'year' and 'final_year' are numeric
  land_coverage_df$year <- as.numeric(land_coverage_df$year)
  final_year <- as.numeric(final_year)
  
  land_coverage_agg <- land_coverage_df %>% 
    # Grouping by ID
    group_by(id) %>%
    # Filtering records between the initial year (2000) and 'final_year'
    filter(year == 2000 | year == final_year) %>%
    # Calculating the percentage variation of 'Total_natural_formation' - first and last year
    summarise(variation_pct = (last(Total_natural_formation) - first(Total_natural_formation)) / 
                first(Total_natural_formation)) %>%
    # Filtering CU that did have a reduction greater than 1% in 'Total_natural_format'
    filter(variation_pct <= criterion) %>% # CHANGE BACK TO <= 
    
    # Return filtered data
    return (land_coverage_agg)
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : df is a dataframe composed with land use coverage data and info about REDD+
# @Arg       : criterion is a number corresponding to the percentage of tolerance 
# @Output    : Dataframe with added columns
# @purpose   : Function to create treatment columns based on certain conditions
# @desc      : The function creates one column for each treated unit with value NA for all
# other treated units and 0 for never treated. The treatment binary columns is based on the
# 'start' column from df and equals 1 for treated units when year >= start (if start is in
# last quarter, it takes the next year as the first treatment year). The function creates
# different columns for each treated unit (with respective id) and three general treatment
# columns
create_treatment_columns <- function(df, years_antecipation) {
  
  # Initial preparation: Determine treated units and the year of treatment start
  treated_info <- df %>%
    filter(!is.na(certification_date)) %>%
    mutate(start_year = year(start),
           treatment_start_year = if_else(month(start) >= 10,
                                          start_year + 1,
                                          start_year),
           certification_year   = if_else(month(certification_date) >= 10,
                                          year(certification_date) + 1,
                                          year(certification_date)),
           anticipation_year    = treatment_start_year - years_antecipation) %>%
    distinct(id_registry, treatment_start_year, certification_year, anticipation_year)
  
  df <- df %>% 
    left_join(treated_info, by = "id_registry")
  
  # Adding direct treatment columns based on treatment criteria
  df <- df %>%
    mutate(
      treatment = ifelse(!is.na(certification_date) & year >= treatment_start_year,
                         1, 0),
      treatment_registered = ifelse(!is.na(certification_date) & year >=  certification_date,
                                    1, 0),
      treatment_anticipation = ifelse(!is.na(certification_date) &
          year >= anticipation_year, 1, 0)
    )
  
  treatment_columns <- c("treatment", "treatment_registered", "treatment_anticipation")
  
  # Create treatment columns for each treated unit
  for (i in seq_along(treated_info$id_registry)) {
    current_id <- treated_info$id_registry[i]
    start_year <- treated_info$treatment_start_year[i]
    
    new_col_name <- paste0("treatment_", current_id)
    
    # Creating the treatment column with specific criteria
    df[[paste0("treatment_", current_id)]] <-
      ifelse(
        df$id_registry == current_id & df$year >= start_year,
        1,
        ifelse(df$id_registry == current_id, 0, NA)
      )
    
    treatment_columns <- c(treatment_columns, new_col_name)
  }
  
  # Update values for units that were never treated or are different from the current analysis
  never_treated_ids <- setdiff(df$id_registry, treated_info$id_registry)
  df <- df %>%
    mutate(across(starts_with("treatment_"), ~ifelse(is.na(.),
                                                     ifelse(id_registry %in% never_treated_ids,
                                                            0, 
                                                            NA),
                                                     .)))
  # Find the position of the "year" column
  year_col_position <- which(names(df) == "year")
  
  # Rearrange columns to position treatment columns right after "year"
  df <- df %>%
    dplyr::select(names(df)[1:year_col_position], all_of(treatment_columns), everything()) 
    # dplyr::select(!c(registry, project_developer, project_description, methodology_version))
  
  # Return df
  return(df)
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : projects_id is a id of APD treated REDD+
# @Arg       : merged_project is a dataframe composed with land use coverage data and REDD+ info
# @Arg       : deforestation_criterion is a value used for filter_no_deforestation
# @Arg       : year is a number used for filter_no_deforestation
# @Output    : Dataframe project with APD information and potential donors
# @purpose   : Function to merge potential donors, projects - based on certain conditions
# @desc      : The function creates finds 
get_treatment_apd_cu <- function(projects_id, merged_project, deforestation_criterion, year){
  
  columns = c("id_registry", "year", "Total_natural_formation", "Forest_pctg",
              "Non_Forest_Natural_pctg", "Farming_pctg", "Water_pctg", "Not_Observed_pctg",
              "start", "certification_date", "state", "project_name", "area_ha")
  
  # Get dataframes for each treated APD projects
  project_panel <- merged_project %>%
    filter(id_registry == projects_id) %>% 
    rename(area_ha = area_hectares) %>% 
    mutate(id_registry = as.character(id_registry)) %>%  # important for merge
    dplyr::select(all_of(columns))
  
  # Get states from project_panel
  states <- project_panel %>%
    dplyr::select(state) %>%
    rowwise() %>%
    mutate(state = list(unlist(strsplit(state, split = "/")))) %>%
    unnest(state) %>% 
    distinct() %>% 
    pull()
  
  # Control for treated APDs
  donors <- filter_no_deforestation(land_coverage_df = merged_conservation,
                                    final_year = year,
                                    criterion = deforestation_criterion) %>%
    right_join(merged_conservation, by = join_by(id)) %>% # get information from CU
    rename(id_registry = id, project_name = name, state = sigla_uf) %>%  # rename columns merge
    dplyr::select(!variation_pct) %>%  # remove unnecessary columns
    mutate(start = NA, certification_date = NA, area_ha = as.double(area_ha)) %>% 
    filter(state %in% states)
  
  # Merge donors with treated project
  merged_df <- full_join(project_panel, donors, by = columns)
  
  # Return final df
  return(merged_df)
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : df is a dataframe of REDD+ projects with their land information
# @Arg       : type is a string (either "APD" or "AUD")
# @Arg       : tolerance is a value specifying the tolerance level about the variation in data
# @Arg       : additional is a logical argument to take projects that are both APD and AUD
# @Output    : Dataframe with filtered type and filtered about little variation projects
# @purpose   : Function to filter type and variation level
# @desc      : The function filter data based on type and amount of variation in the outcome
# component
filter_type_variation <- function(df, type, tolerance, additional = FALSE){
  
  # Filter data by redd type
  if(additional){
    data <- df %>%
      filter(redd_type == type | redd_type == "AUD/APD")
  } else{
    data <- df %>%
      filter(redd_type == type)
  }
    
  
  # Identify donors with no/low variation - reduction donor pool may be important
  donors_no_var <- data %>%
    group_by(id_registry) %>%
    filter(diff(range(Total_natural_formation)) <= tolerance) %>% # 0.0005 all time
    distinct(id_registry) %>%
    ungroup() %>% 
    pull(id_registry)
  
  data <- data[!data$id_registry %in% donors_no_var, ]
  
  return(data)
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : df is a dataframe of REDD+ projects with their land information
# @Arg       : additional is a logical argument to take projects that are both APD and AUD
# @Output    : Dataframes with one treated and all correct potential donors
# @purpose   : Function to create single dataframes
# @desc      : The function finds the treated units and sets appropriates donors
create_treatment_dfs <- function(df, additional = FALSE) {
  # Identify treatment columns
  treatment_cols <- grep("^treatment_", names(df), value = TRUE)
  
  # Extract registry IDs from treatment columns and filter to keep only numeric ones
  treatment_ids <- gsub("treatment_", "", treatment_cols)
  treatment_ids <- treatment_ids[grepl("^\\d+$", treatment_ids)]
  
  for (id in treatment_ids) {
    # Dynamic name for each dataframe
    df_name <- paste0("verra_", id)
    
    # Get the redd_type of the treated unit
    treated_redd_type <- df %>% 
      filter(id_registry == id) %>% 
      dplyr::select(redd_type) %>% 
      unique() %>%
      pull(redd_type)
    
    # Filter for the treated unit and controls with the same redd_type (never treated)
    # if Additional is True, we also take AUD/APD projects
    if(additional){
      filtered_df <- df %>%
        filter(redd_type == treated_redd_type | redd_type == "AUD/APD") %>%
        filter((get(paste0("treatment_", id)) == 1) | 
                 get(paste0("treatment_", id)) == 0 & id_registry == id |
                 rowSums(is.na(dplyr::select(., starts_with("treatment_"))) |
                           dplyr::select(., starts_with("treatment_")) == 0,
                         na.rm = TRUE) == length(treatment_cols)) %>%
        dplyr::select(-one_of(treatment_cols)) # Optionally remove treatment columns
    } else {
      filtered_df <- df %>%
        filter(redd_type == treated_redd_type) %>%
        filter((get(paste0("treatment_", id)) == 1) | 
                 get(paste0("treatment_", id)) == 0 & id_registry == id |
                 rowSums(is.na(dplyr::select(., starts_with("treatment_"))) |
                           dplyr::select(., starts_with("treatment_")) == 0,
                         na.rm = TRUE) == length(treatment_cols)) %>%
        dplyr::select(-one_of(treatment_cols)) # Optionally remove treatment columns
    }
    
    # Assign the filtered dataframe to a dynamically named variable in the global environment
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : conservation_df is a dataframe containing information about conservation units
# @Arg       : verra_df_names is a list dataframes of treated units APD projects (each)
# @Output    : Assigns for the dataframes in the list the merged data
# @purpose   : Function to merge two datasets by clearing the "nome" column from the first argue
# @desc      : The function first treat "nome" column, merge and add new information about the
# year in which conservation units were created. The function also removes any "FLORESTA", 
# "ESTAÇÃO" or "PARQUE" from dataset
merge_filter_clean <- function(conservation_df, verra_df_names) {
  
  # First, clean and prepare conservation units
  conservation_df_cleaned <- conservation_df %>%
    mutate(start = as.character(ano_cria), # Guarantee start as character
           start = gsub(".*?(\\d{4}).*", "\\1", start), # take year
           start = ymd(paste0(start, "-01-01")), .after = esfera) %>% # Converts to Date
    filter( # filter values
      !grepl("FLORESTA", nome, ignore.case = TRUE) &
      !grepl("ESTAÇÃO|ESTAçãO", nome, ignore.case = TRUE) &
      !grepl("PARQUE", nome, ignore.case = TRUE)
    )
    
    
  for(df_name in verra_df_names) {
    
    # Dynamically loads the verra_* dataframe
    verra_df <- get(df_name) %>%
      filter( # filter values
        !grepl("FLORESTA", project_name, ignore.case = TRUE) &
        !grepl("ESTAÇÃO|ESTAçãO", project_name, ignore.case = TRUE) &
        !grepl("PARQUE", project_name, ignore.case = TRUE))
    
    # Merge verra df with conservation_units
    merged_df <- verra_df %>%
      left_join(conservation_df_cleaned, by = c("project_name" = "nome"))
    
    #  Update the "start" column in the original verra_df only where it was NA before
    verra_df$start <- coalesce(verra_df$start, merged_df$start.y)
    
    # Saves the updated dataframe back to the global environment with the same name
    assign(df_name, verra_df, envir = .GlobalEnv)
  }
}

# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : treatment_df is a dataframe containing one treated and its potential donors
# @Arg       : acceptable_variation is a number about the minimum amount of variation in data
# @Arg       : outcome is a string representing the column consider as the outcome
# @Arg       : cov_adj is a list of covariates to be adjusted
# @Arg       : convervation_donors is a logical value about using conservation units
# @Output    : Returns a list of class "scpi" that have the synthetic results
# @purpose   : Function estimate single unit synthetic controls
# @desc      : The function first get information about the treated unit, filter donors with
# low amount of variation, and then estimate synthetic control using scpi package
# See https://github.com/nppackages/scpi/blob/main/R/
estimate_unit_synthetic <- function(treatment_df, acceptable_variation, outcome, cov_adj,
                                    conservation_donors = FALSE){
  # Get information about the treated project
  treated_information <- treatment_df %>% 
    filter(!is.na(certification_date)) %>% # enable to find the treated value in treatment_df
    mutate(start_year = year(start),
           treatment_start_year = if_else(month(start) >= 10,
                                          start_year + 1,
                                          start_year),
           certification_year   = if_else(month(certification_date) >= 10,
                                          year(certification_date) + 1,
                                          year(certification_date))) %>%
    distinct(id_registry, treatment_start_year, certification_year)
  
  # Get information about the treated unit
  treated_year         <- treated_information$treatment_start_year
  treated_id           <- treated_information$id_registry
  treated_registration <- treated_information$certification_year
  
  # Filter donors
  donors <- treatment_df %>%
    filter(is.na(certification_date))
  
  # Filter treated
  values_id <- treatment_df %>% 
    filter(!is.na(certification_date))
  
  # Find good donors
  donors_vars <- donors %>%
    filter(year >= 2000 & year <= treated_year) %>%
    group_by(id_registry) %>%
    summarise(variation = diff(range(Total_natural_formation)),
              value_zero = any(Total_natural_formation == 0)) %>%
    filter(variation >= acceptable_variation & !value_zero) %>%
    pull(id_registry)
  
  # Remove bad donors
  donors <- donors[donors$id_registry %in% donors_vars, ]
  
  # Create main dataframe
  data <- rbind(values_id, donors)
  
  # List of features
  # list_features <- list(c(outcome,"area_ha"))
  
  ### Set options for data preparation
  id.var            <- "id_registry"                              # ID variable
  time.var          <- "year"                                     # Time variable
  period.pre        <- seq(from = first(data$year), to = treated_year, by = 1)
  period.post       <- seq(from = treated_year + 1, to = last(data$year))
  unit.tr           <- treated_id                                 # Treated unit
  unit.co           <- setdiff(unique(data$id_registry), unit.tr) # Donors pool
  outcome.var       <- outcome                                    # Outcome variable
  cov.adj           <- cov_adj                                    # Covariates for adjustment
  features          <- outcome                                    # No features != outcome
  constant          <- FALSE                                      # No constant term
  report.missing    <- TRUE                                       # To check missing values 
  cointegrated.data <- TRUE                                       # Belief data cointegrated
  verbose           <- TRUE
  anticipation      <- 0
  
  # Define new covariates for APD with conservation units donors
  if(conservation_donors){
    features <- c("Total_natural_formation", "area_ha")
    cov.adj  <- list("Total_natural_formation" = c("constant", "trend"),
                     "area_ha" = c("constant"))
  }
  
  # Data preparation
  df  <-
    scdata(df                = data,
           id.var            = id.var,
           time.var          = time.var,
           outcome.var       = outcome.var,
           period.pre        = period.pre,
           period.post       = period.post,
           unit.tr           = unit.tr,
           unit.co           = unit.co,
           cov.adj           = cov.adj,
           features          = features,
           constant          = constant,
           cointegrated.data = cointegrated.data,
           anticipation      = anticipation,
           verbose           = verbose)
  
  # Set options for inference
  u.alpha  <- 0.05                         # Confidence level (in-sample uncertainty)
  e.alpha  <- 0.05                         # Confidence level (out-of-sample uncertainty)
  rho      <- NULL                         # Regularization parameter (if NULL it is estimated)
  rho.max  <- 1                            # Maximum value attainable by rho
  sims     <- 200                          # Number of simulations
  V        <- NULL                         # Weighting matrix (if NULL it is the identity mtrx)
  u.order  <- 1                            # Degree of polynomial in B and C when modelling u
  u.lags   <- 0                            # Lags of B to be used when modelling u
  u.sigma  <- "HC1"                        # Estimator for the variance-covariance of u
  u.missp  <- T                            # If TRUE then the model is treated as misspecified
  e.lags   <- 0                            # Degree of polynomial in B and C when modelling e
  e.order  <- 1                            # Lags of B to be used when modelling e
  e.method <- "gaussian"                   # Estimation method for out-of-sample uncertainty
  cores    <- 3                            # Number of cores to be used by scpi
  w.constr <- list(name = "simplex")       # Simplex-type constraint set
  
  
  # Implements estimation and inference procedures for Synthetic Control (SC
  result  <- scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, 
                  u.missp = u.missp, sims = sims, e.order = e.order, e.lags = e.lags,
                  e.method = e.method, cores = cores, w.constr = w.constr, u.alpha = u.alpha,
                  e.alpha = e.alpha, rho = rho, rho.max = rho.max)  
  
  print(result)
  # return estimated information
  return(result)
}


# Function --------------------------------------------------------------------
# @written_on: 8/02/2024
# @written_by: Marcos Paulo
# @Arg       : result is a list of class "scpi" that have the synthetic results
# @Arg       : treatment_df is a dataframe containing one treated and its potential donors
# @Arg       : lower is a number of the lower limit of y scale of plot
# @Arg       : upper is a number for the upper limit of y scale of plot
# @Arg       : redd_height is a number for the height of REDD+ text
# @Arg       : certification_height is a number for the height of certification text
# @Output    : Returns a plot
# @purpose   : Function to generate plots for synthetic estimations
# @desc      : The function first get information about the synthetic prediction, actual outcome
# lower and upper bound (and others) and then create a dataframe with then. The dataframe is
# used to create a plot. Code based on scpi package recommendations
# See https://github.com/nppackages/scpi/blob/main/R/
generate_plots_synthetic <- function(result, treatment_df, lower, upper, redd_height,
                                     certification_height){
  # Get the name of the result dataframe
  name      <- deparse(substitute(treatment_df))
  data_name <- sub(".*_", "", name)
  # Get certification year
  certification_year <- treatment_df %>% 
    dplyr::select(certification_date) %>% 
    dplyr::filter(!is.na(certification_date)) %>%
    distinct() %>% 
    pull() %>% 
    year(.)
  
  # Store data on treated unit, synthetic unit, and prediction bars
  y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
  y.act <- rbind(result$data$Y.pre, result$data$Y.post)
  
  sc.l  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
  sc.r  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
  
  # Store other specifics
  period.pre  <- result$data$specs$period.pre
  period.post <- result$data$specs$period.post
  T0          <- period.pre[length(period.pre)] # intercept
  plot.range  <- c(period.pre, period.post)
  
  # Actual data
  dat    <- data.frame(t     = c(period.pre, period.post),
                       Y.act = c(y.act),
                       sname = "Treated")
  
  # Fill with NAs Y.fit and confidence bounds where missing
  Y.fit.na  <- matrix(NA, nrow = length(c(period.pre, period.post)))
  sc.l.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
  sc.r.na   <- matrix(NA, nrow = length(c(period.pre, period.post)))
  
  names <- strsplit(rownames(y.fit), "\\.")
  not.missing.plot <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
  names <- strsplit(rownames(sc.l), "\\.")
  not.missing.ci   <- c(period.pre,period.post) %in% unlist(lapply(names, "[[", 2))
  
  Y.fit.na[not.missing.plot, 1] <- y.fit
  sc.l.na[not.missing.ci, 1]    <- sc.l
  sc.r.na[not.missing.ci, 1]    <- sc.r
  
  # Synthetic unit data
  dat.sc <- data.frame(t        = c(period.pre, period.post),
                       Y.sc     = Y.fit.na,
                       lb       = c(sc.l.na), ub = c(sc.r.na),
                       sname    = "SC Unit")
  
  # Set ticks, event label and merge
  # x.ticks <- c(seq(plot.range[1], plot.range[length(plot.range)], length.out = 5), T0)
  # x.ticks <- round(unique(x.ticks))
  
  event.lab <- paste("\n", "REDD+", sep = "")
  event.lab.height <- redd_height
  
  certification <- paste("\n", "certification", sep = "")
  certification_height <- certification_height
  
  dat.plot    <- subset(dat,    t %in% plot.range)
  dat.sc.plot <- subset(dat.sc, t %in% plot.range)
  
  plotdf <- dplyr::left_join(dat.plot, dat.sc.plot, by = 't') %>% 
    mutate(Y.act = Y.act / 100,
           Y.sc = Y.sc / 100,
           lb = lb / 100,
           ub = ub / 100)
  
  # Plot specs
  plot <- ggplot() + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.3, hjust=1),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black")) +
    labs(title = paste(data_name)) +
    labs(x = "Year", y = "Natural formation (in percentage)") +
    theme(legend.position = "bottom", legend.box = "horizontal",
          legend.background = element_rect(fill = "white", color = "black"))
  
  # Add Series to plot
  plot <- plot +
    geom_line(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), linetype = 'solid') +
    geom_point(data = plotdf, aes(x = t, y = Y.act, colour = sname.x), shape = 16) +
    geom_line(data = plotdf, aes(x = t, y = Y.sc, colour = sname.y), linetype = 'dashed') +
    geom_point(data = plotdf, aes(x = t, y = Y.sc, colour = sname.y), shape = 2) +
    geom_vline(xintercept = T0, linetype = "dashed", colour = "black") +
    geom_text(aes(x = T0, label = event.lab, y = event.lab.height), angle = 90, size = 9) +
    geom_vline(xintercept = certification_year, linetype = "dotted") +
    geom_text(aes(x = certification_year, label = certification, y = certification_height),
              angle = 90, size = 9) +
    scale_y_continuous(limits = c(lower, upper), labels = percent_format()) +
    scale_color_manual(name = "", values = c("mediumblue", "black"),
                       labels = c("Synthetic Control", "Treated"),
                       guide = guide_legend(override.aes = list(
                         linetype = c('solid','solid'), shape = c(2, 16)))) +
    geom_errorbar(data = plotdf %>% filter(!is.na(lb) & !is.na(ub)),
                  aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                  width = 0.2, linetype = 2, linewidth = 0.5,
                  position = position_dodge(0.05)) +
    geom_linerange(data = plotdf %>% filter(!is.na(lb) & !is.na(ub)),
                   aes(x = t, ymin = lb, ymax = ub, colour = sname.y),
                   position = position_dodge(0.05), linewidth = 0.5)
  
  
  # Final plot characteristics
  plot <- plot +
  theme(text = element_text(size = 20, color = "black"), # Adjust size of text
        axis.title = element_text(size = 22, color = "black", face = "bold"), # Title of axis
        axis.text = element_text(size = 20, color = "black", face = "bold"), # Text in axis
        legend.title = element_blank(), # Legend title
        legend.text = element_text(size = 20, color = "black"), # Text in legend
        legend.key.size = unit(2.5, "lines")) # size of symbols in legend
  
  
  # Return plot
  return(plot)
}



# Function to divide the list of plots into subsets
split_list <- function(list, n) {
  lapply(seq(1, length(list), by = n), function(i){
    list[i:min(i+n-1, length(list))]
  })
}

