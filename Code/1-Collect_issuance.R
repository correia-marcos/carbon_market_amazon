# =============================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# =============================================================================
# Goal: Collect more information about carbon projects
# 
# We already create a dataset with virtually all carbon offsets in Brazil. Now,
# we want to open all excel files from Data/Projects_info/Issuances in order to
# collect information about year, amount of credits and additional certification
# that each Verra registered project have.
# 
# This script was implemented to:
#   1 - Open all issues excel files and collect all important information 
#   2 - Merge all datasets into one dataframe and save it as projects_issues.csv
#   3 - Add all certifications info on the base_projects.csv file
# 
# @Date: nov 2023
# @author: Marcos

# Required for increasing reproducibility
library(groundhog)            # You need to have at least R version = 4.3.1

# Required packages 
pkgs <- c("dplyr", "readxl", "lubridate", "data.table", "stringr")

# Load packages
groundhog.library(pkgs, "2023-09-01")

# Import data: excel previous created by author
data <- readxl::read_excel("Data/Projects_info/base_projects.xlsx",
                           na = "--")
# List of of files with issues information
issues_files <- list.files("Data/Projects_info/Issuances", full.names = TRUE)

# Function --------------------------------------------------------------------
# written on: 11/12/2023
# written by: Marcos Paulo
# Input     : Vector with full name of the excel files
# Output    : Dataframe with 5 columns in a tidy data format
# purpose   : Read project issues, process it and create a tidy version by year
# desc      : We used information in issues datasets to collect information
# about issues amount, year of vintage, year of crediting, year of retirement,
# and ID. We then create a tidy dataframe containing year, ID, and credits
process_project_info <- function(file_path) {
  
  # Read data from Excel file
  project_data <- read_excel(file_path)
  
  # Substituting white spaces per underline
  colnames(project_data) <- gsub(" ", "_", colnames(project_data))

  # Filter dataset
  filtered_data <- project_data %>%
    dplyr::select(c("Issuance_Date", "Vintage_Start", "ID",
                    "Retirement/Cancellation_Date", "Quantity_Issued"))
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
  final_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list_df)
  
  # Return the final dataframe
  return(final_df)
}

# Function --------------------------------------------------------------------
# written on: 12/12/2023
# written by: Marcos Paulo
# input     : Dataframe and a list of Excel files to loop over
# output    : Dataframe with those columns specific columns modified 
# purpose   : Read project issues, save certifications and add info into df
# desc      : We used information in issues datasets to collect information
# about the type of certification a project has. Once we collect all unique
# certifications, we mutate the value of a column representing that
# certification to 1 and 0 otherwise. The function goes over all Excel files
add_certifications <- function(df, files_list) {
  
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

# Applying functions ---------------------------------------------------------


# First function application: 

# Loop through files and collect data
all_project_data <- lapply(issues_files, process_project_info)

# Combine data from all files and save dataframe
projects_issues <- rbindlist(all_project_data)
write.csv(projects_issues, file = "Data/Projects_info/projects_issuance.csv",
          row.names = FALSE)


# Second function application:

# Loop through Excel files and Save dataframe
updated_data <- add_certifications(data, issues_files) 
write.csv2(updated_data, file = "Data/Projects_info/base_projects.csv",
           row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE)
