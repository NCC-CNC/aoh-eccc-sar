library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(fasterize)
library(raster)

## This script rolls up the 30m AOH maps to the NCC 1KM grid cell size 
## The resulting rasters are continuous rasters with each pixel value representing the number of hectares within each 1km cell
start_time <- Sys.time()

folder_path <- list.files("C:/aoh-eccc-sar/Data/AOH_SAR_Layers", pattern=".tif$", full.names = T)

# Read-in national 1km raster grid
NCC_1KM_GRID <- raster("C:/aoh-eccc-sar/Data/boundaries_ncc/NCC_1KM_IDX.tif")


for (i in folder_path) {
# Read-in 30m pixels (source)
AOH_30m <- rast(i)


# Project national 1km vector grid 
NCC_1KM_IDX_PROJECTION <-  read_sf("C:/aoh-eccc-sar/Data/boundaries_ncc/NCC_1KM_IDX.shp") %>%
  st_transform(crs = terra::crs(AOH_30m)) %>%
  st_make_valid()
# Calculate vector grid area
NCC_1KM_IDX_PROJECTION$AREA_m2 <- st_area(NCC_1KM_IDX_PROJECTION) 

# Sum 30m AOH pixels that intersect 1km national vector grid

NCC_1KM_IDX_PROJECTION$AOH_COUNT <- exact_extract(AOH_30m, NCC_1KM_IDX_PROJECTION, fun= 'sum')


# Convert 30m AOH pixel sum to proportion

NCC_1KM_IDX_ALBERS_AOH <-  NCC_1KM_IDX_PROJECTION %>%
  filter(AOH_COUNT > 0) %>%
  mutate(AOH_m2 = AOH_COUNT * 900) %>%
  mutate(AOH_PCT = as.numeric(round(((AOH_m2 / AREA_m2) * 100),1))) %>%
  mutate(AOH_PCT = if_else(AOH_PCT > 100, 100, AOH_PCT)) %>%
  st_transform(crs = st_crs(NCC_1KM_GRID)) # back to Canada_Albers_WGS_1984

# Polygon to Raster
AOH_1KM <- fasterize(NCC_1KM_IDX_ALBERS_AOH, NCC_1KM_GRID, field = "AOH_PCT") 
writeRaster(rast(AOH_1KM), filename = paste0("C:/aoh-eccc-sar/Data/AOH_SAR_Layers_1KM/", names(AOH_30m),"_1KM.tif"), overwrite = TRUE, datatype="FLT4S")
}

## End timer
end_time <- Sys.time()
end_time - start_time 