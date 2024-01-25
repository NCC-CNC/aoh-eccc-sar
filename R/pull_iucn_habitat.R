#
# Authors: Dan Wismer
#
# Date: Sept 26th, 2023
#
# Description: Pulls IUCN Habitat descriptions via IUCN Red List API
#
# Inputs: ECCC SAR range map extent .csv  
#
# Output: .csv with ECCC SAR and associated IUCN habitat descriptions.
#
# Note:  1. Need IUCN Red List API token: https://apiv3.iucnredlist.org/. 
#           This token needs to be copied to a .Renviron file:
#           IUCN_REDLIST_KEY = "your token"
#        2. Half of the species list did not have IUCN habitat information.
#           These species get coded -9999
#
# ETA: ~ 28 minutes
# Tested on R Version(s): 4.3.1
#===============================================================================

library(rredlist)

## Start timer
start_time <- Sys.time()

# Read-in ECCC SAR csv
sar_csv <- read.csv("Data/ECCC/Species-at-Risk-RangeMap-Extents.csv")

# Create empty df to populate
df <- data.frame(
  COSEWICID = numeric(),
  SCI_NAME = character(),
  COM_NAME = character(),
  TAXON = character(),
  SAR_STAT = character(),
  SCHEDULE = character(),
  CODE = numeric(),
  HABITAT = character(),
  SUITABILITY = character(),
  SEASON = character(),
  MAJ_IMPORTANCE = character()
)

# Loop over each row and populate new df
for (row in 1:nrow(sar_csv)) {
  ## eccc metadata
  cowsewicid <- sar_csv[row, "COSEWICID"]
  sci_name <- sar_csv[row, "SCI_NAME"]
  com_name <- sar_csv[row, "COM_NAME_E"]
  taxon <- sar_csv[row, "TAXON_E"]
  sar_stat <- sar_csv[row, "SAR_STAT_E"]
  schedule <- sar_csv[row, "SCHEDULE_E"]
  
  ## message
  print(paste0(sci_name, " (", row, "/",  nrow(sar_csv), ")"))
  
  ## IUCN habitat
  query <- tryCatch({
    rl_habitats(sci_name)
  }, error = function(e) {
    print(e)
    return(NULL)
  }) 
    
  ### empty query
  if (length(query$result) == 0 || is.null(query)) {
    code = -9999
    habitat = "NA"
    suitability = "NA"
    season = "NA"
    maj_importance = "NA"
    
    ### construct new row with empty IUCN info
    new_row <- c(
      cowsewicid, sci_name, com_name, taxon, sar_stat, schedule,
      code, habitat, suitability, season, maj_importance
    )
    # populate df
    df <- structure(rbind(df, new_row), .Names = names(df))
    # write to csv
    write.csv(df, "ECCC_SAR_IUCN_HABITAT.csv", append = TRUE)
    
  } else {
    ### extract records
    for (row in 1:nrow(query$result)) {
      code <- query$result[row, "code"]
      habitat <- query$result[row, "habitat"]
      suitability <- query$result[row, "suitability"]
      season <- query$result[row, "season"]
      maj_importance <- query$result[row, "majorimportance"]
      
      ### construct new row
      new_row <- c(
        cowsewicid, sci_name, com_name, taxon, sar_stat, schedule,
        code, habitat, suitability, season, maj_importance
      )
      # populate df
      df <- structure(rbind(df, new_row), .Names = names(df))
      # write to csv
      write.csv(df, "ECCC_SAR_IUCN_HABITAT.csv", append = TRUE)
    }
  }
}

# End timer
end_time <- Sys.time()
end_time - start_time
