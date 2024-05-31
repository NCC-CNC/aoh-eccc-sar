#
# Authors: Nikol Dimitrov
#
# Date: May 30, 2024
#
# Description: Pulls AOH identities and hectares for provided projects 
#
# Inputs: Project shapefiles
#         AOH maps rolled up to 1KM grid 
#         SAR Status information from May 2024 SARA list pull
#
#
# Note: Some statuses may need to be manually checked with SARA list: 
#       NA values on list pull may not be NA on web version 
#       AOH maps use 2016 SAR information - some of those species may no longer
#       be at risk, and would need to be removed 
#
# Output: .csv with provided projects and associated AOH metrics
#
# ETA: ~ less than 2 minutes
# Tested on R Version(s): 4.3.1
#===============================================================================

library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(raster)
library(tidyr)
library(stringr)

## Start timer
start_time <- Sys.time() 

setwd("C:/aoh-eccc-sar")

#Set projection for projects 
prj <- rast("./AOH_SAR_NULL_1KM/COSEWICID_1004_1KM.tif")

#Load AOH rasters
aoh_1km <-  list.files("./Data/AOH_SAR_Layers_1KM", pattern=".tif$", full.names = T)

# Read in projects 
NHCP_projects <- read_sf("./Data/NHCP/NHCP_2023_Y1_Priority.shp") %>% 
  st_transform(crs = terra::crs(prj)) %>%
  st_make_valid()

# Loop through each AOH layer, calculate area, and record COSEWICID
for (i in aoh_1km){
  aoh <- rast(i)
  name <-terra::sources(aoh)
  name <- gsub(".*/","",name)
  name <- gsub("\\..*","", name) 
  
  NHCP_projects[[paste0(name, "_aoh_area")]] <- exact_extract(aoh, NHCP_projects, fun= "sum")
}

# Convert sf object to tibble 
projects <- as_tibble(NHCP_projects) %>%
  dplyr::select(-c(geometry))

# Gather projects by COSEWICID and record suitable habitat in column 
# Filter for records where suitable habitat was not 0 
projects_clean <- gather(projects, COSEWICID, `Suitable habitat for SAR (Hectares)`, COSEWICID_1004_1KM_aoh_area:COSEWICID_999_1KM_aoh_area) %>% 
  filter(`Suitable habitat for SAR (Hectares)` != 0)

# Clean up COSEWIC column
projects_clean <- projects_clean %>%
  mutate_at("COSEWICID", str_replace, "COSEWICID_", "") %>% 
  mutate_at("COSEWICID", str_replace, "_1KM_aoh_area", "") 

projects_clean$COSEWICID <- as.double(projects_clean$COSEWICID)

# Add cosewic info 
sar_list <- read_csv("C:/aoh-eccc-sar/Data/SAR Status Information/sar_range_maps.csv")
sar_updated_status <- read_csv("C:/aoh-eccc-sar/Data/SAR Status Information/2023_SAR_Species.csv")

sar_updated_status <- sar_updated_status %>% 
  dplyr::select(`Scientific name`, `COSEWIC status`, `Schedule status`, cosewic_match)

sar_list <- sar_list[, 2:6]
sar_list <- sar_list[!duplicated(sar_list),]

# Join species information: scientific name, common name EN, common name FR
projects_clean_join <- left_join(projects_clean, sar_list, by = "COSEWICID")

# Join species information: SARA stauts, COSEWIC status 
projects_clean_join_status <- left_join(projects_clean_join, sar_updated_status, by=c("COM_NAME_E" = "cosewic_match"))

# Write csv
write_csv(projects_clean_join_status, "NHCP_AOH_Projects.csv")

## End timer
end_time <- Sys.time()
end_time - start_time 
