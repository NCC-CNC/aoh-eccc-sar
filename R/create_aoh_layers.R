library(readr)
library(tidyverse)
library(raster)
library(terra)
library(sf)
setwd("C:/aoh-eccc-sar")

manual_classifications <- read_csv("./Data/Habitat Classification/manually_classified_species_CORRECTION.csv")

IUCN_classifications <- read_csv("./Data/Habitat Classification/IUCN_classified_species_CORRECTION.csv")
IUCN_associations <- read_csv("./Data/Habitat Classification/IUCN_species_associations.csv")


IUCN_merged <- left_join(IUCN_associations, IUCN_classifications, by= 'HABITAT')

IUCN_grouped <- IUCN_merged %>% 
  group_by(COSEWICID) %>% 
  mutate(`Land Cover Class` = paste(`Land Cover Class`, collapse = ' '),
         `Land Cover Value` = paste(`Land Cover Value`, collapse = ' '),
         `Comments/Notes (AF Jan 2024)` = paste(`Comments/Notes (AF Jan 2024)`, collapse = ' ')) %>% 
  slice(1)

IUCN_grouped <- IUCN_grouped[,-1]
IUCN_grouped$CODE <- as.double(IUCN_grouped$CODE)
SAR_dataframe <- rbind(IUCN_grouped, manual_classifications)


SAR_dataframe_grouped <- SAR_dataframe %>% 
  group_by(COSEWICID) %>% 
  mutate(`Land Cover Class` = paste(`Land Cover Class`, collapse = ' '),
         `Land Cover Value` = paste(`Land Cover Value`, collapse = ' '),
         `Comments/Notes (AF Jan 2024)` = paste(`Comments/Notes (AF Jan 2024)`, collapse = ' '),
         `HABITAT_SOURCE` = paste(`HABITAT_SOURCE`, collapse = ' '),
         `HABITAT` = paste(`HABITAT`, collapse = ' ')) %>% 
  slice(1)

SAR_dataframe_grouped$`Land Cover Value` <- str_replace_all(SAR_dataframe_grouped$`Land Cover Value`, ",", " ")
SAR_dataframe_grouped$`Land Cover Value` <- str_replace_all(SAR_dataframe_grouped$`Land Cover Value`, " ", ",")
SAR_dataframe_grouped$`Land Cover Value` <- str_replace_all(SAR_dataframe_grouped$`Land Cover Value`, ",,", ",")
SAR_dataframe_grouped$`Land Cover Value` <- str_replace_all(SAR_dataframe_grouped$`Land Cover Value`, ",", " ")

SAR_dataframe_grouped$`Land Cover Value` <- str_replace_all(SAR_dataframe_grouped$`Land Cover Value`, "NA", "-9999")
SAR_dataframe_grouped$`Land Cover Value` <- strsplit(SAR_dataframe_grouped$`Land Cover Value`, " +")
SAR_dataframe_grouped$`Land Cover Value` <- lapply(SAR_dataframe_grouped$`Land Cover Value`, unlist)
SAR_dataframe_grouped$`Land Cover Value` <- lapply(SAR_dataframe_grouped$`Land Cover Value`, as.numeric)
SAR_dataframe_grouped$`Unique Land Cover Value` <- lapply(SAR_dataframe_grouped$`Land Cover Value`, unique)
SAR_dataframe_grouped$`Unique Land Cover Value` <- lapply(SAR_dataframe_grouped$`Unique Land Cover Value`, setdiff, -9999)


SAR_dataframe_grouped$COSEWICID <- sub("^", "COSEWICID_", SAR_dataframe_grouped$COSEWICID)


sar_ranges <- list.files("./Data/ECCC/SAR_ranges", pattern=".shp$", full.names = T)
landcover_data <- raster("./Data/NRCAN/landcover-2020-projected.tif")


for (i in sar_ranges) {
  range <- st_read(i) %>%
    st_as_sf()
  masked_landcover <- landcover_data %>% 
    crop(range) %>% 
    mask(range)
  
  writeRaster(masked_landcover, filename = paste0("./Data/Masked Landcover/COSEWICID_",as.character(range$COSEWICID), ".tif"), overwrite=TRUE)
  
  temp_fold <- './temp_HMs_to_REMOVE'
  
  # get all files in the directories, recursively
  f <- list.files(temp_fold, include.dirs = F, full.names = T, recursive = T)
  # remove the files
  file.remove(f)
}


masked_ranges <- sar_ranges <- list.files("./Data/Masked Landcover", pattern=".tif$", full.names = T)

for (i in masked_ranges) {
test_range <- rast(i)

values <- SAR_dataframe_grouped[SAR_dataframe_grouped$COSEWICID == names(test_range),] %>% 
  ungroup() %>% 
  dplyr::select(`Unique Land Cover Value`) %>%
  unlist() %>% 
  as.vector()

if (length(values) == 0){
  next
}

reclass_factor <- cbind(values, 1)

r1 <- classify(test_range, reclass_factor, others=NA)

writeRaster(r1, filename = paste0("./Data/Reclassified Landcover Correct/", names(test_range),".tif"), datatype = 'INT2S', overwrite=TRUE)
}

