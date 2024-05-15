### AOH Maps NHCP Calculations 

library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(fasterize)
library(raster)

setwd("C:/aoh-eccc-sar/Data/sar_calculations")

aoh_richness_layer <- rast("./sar_aoh_richness_1km.tif")


aoh_richness_layer
NHCP_projects <- read_sf("./NHCP_2023_Y1_Projects_prj.shp") %>% 
  st_transform(crs = terra::crs(aoh_richness_layer)) %>%
  st_make_valid()

# count number of species 
NHCP_projects$SAR_Count <- exact_extract(aoh_richness_layer, NHCP_projects, fun = "max")

# count area of species 
NHCP_projects$SAR_Area <- exact_extract(aoh_richness_layer, NHCP_projects, fun= "count")
NHCP_projects$SAR_Area <- NHCP_projects$SAR_Area * 100
NHCP_projects$SAR_Area_difference <- round(NHCP_projects$SAR_Area - NHCP_projects$area_ha, digits= 0)

sum(NHCP_projects$SAR_Count)


## species identities 
NHCP_SAR <- read_sf("./NHCP_2023_Y1_SAR.shp")

SAR_COUNTS <- NHCP_SAR %>% 
  group_by(PROJECT_NA) %>% 
  summarise(distinct_SAR = n_distinct(COSEWICID)) %>% 
  as.data.frame() 


n_distinct(NHCP_SAR$COSEWICID)
