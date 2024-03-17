# ============================================================================================
# Dissertation: Can carbon market protect the Amazon: Evidence from Brazil
# ============================================================================================
# @Goal: Collect more information about carbon projects
# 
# @Description: We create a dataset with virtually all carbon projects in Brazil. Now, we want
# to open all excel files from data/Projects_info/Issuances in order to collect information
# about the year, the amount of credits and the additional certification that each Verra
# registered project have.
#
# @Summary: This program intends to
#   1 - Open all issues excel files and collect all important information 
#   2 - Merge all datasets into one dataframe and save it as projects_issues.csv
#   3 - Add all certifications info on the base_projects.csv file
#   4 - Process the dataset (column type, column name changes...)
#
# @Date: nov 2023
# @author: Marcos

# Get all libraries and functions
source(here::here("src", "config_utils.R"))

# ============================================================================================
# I: Import data
# ============================================================================================
# Import data: excel previous created by author
data <- readxl::read_excel(here::here("data", "Projects_info", "base_projects", "full.xlsx"),
                           na = "--")
# List of of files with issues information
issues_files <- list.files(here::here("Data", "Projects_info", "Issuances"),
                           full.names = TRUE)

# ============================================================================================
# II: Process data
# ============================================================================================
# Applying functions from 'src/config_utils.R'

# Loop through files and combine data from all files
all_project_data <- lapply(issues_files, process_project_info)
projects_issues <- data.table::rbindlist(all_project_data)

# Loop through Excel files and Save dataframe
updated_data <- add_certifications(data, issues_files)

# Substituting white spaces per underline on updated_data
colnames(updated_data) <- gsub(" ", "_", colnames(updated_data))

# Change column names and convert data informative columns into Date format
updated_data <- updated_data %>%
  rename(
    start = project_start_crediting,
    end   = project_end_crediting) %>%
  mutate(
    start              = as.Date(start, format = "%m/%d/%Y"),
    end                = as.Date(end, format = "%m/%d/%Y"),
    certification_date = as.Date(certification_date, format = "%m/%d/%Y")
  )

# Remove white spaces from character columns
updated_data <- updated_data %>%
  mutate_if(is.character, str_trim)

# ============================================================================================
# III: Save processed data
# ============================================================================================
readr::write_csv(
  x            = updated_data,
  file         = paste0(here::here("results", "projects_base"), "/base_projects.csv"),
  na           = "NA",
  col_names    = TRUE,
  quote        = "needed")

readr::write_csv(
  x            = projects_issues,
  file         = paste0(here::here("results", "projects_base"), "/projects_issuance.csv"),
  na           = "NA",
  col_names    = TRUE
)
