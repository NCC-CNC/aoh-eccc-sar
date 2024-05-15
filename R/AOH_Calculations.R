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

write_csv("./SAR_AOH_NHCP_Counts.csv")
