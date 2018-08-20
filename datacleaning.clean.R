# Data cleaning: LW; Pools 4, 8, and 13.
# Kaija Gahm
# 11 August 2018

# Source libraries and own functions
source("ownfunctions.R")
source("libraries.R")

# Load datasets
load("data/fish_data_EF.Rda") # Fish sampling data
sites_p4p8p13 <- read.csv("data/DataSets_7_7/AttributeTables/sites_p4p8p13.txt") # reprojected data
rivmi <- read.table("data/p4p8p13_rivmile.txt", sep = ",", header = T) # river mile data
sites_aa_5m <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas5m.txt") # merged aquatic areas data for points buffered by 5 meters.
sites_terrestrial <- read.csv("data/DataSets_7_7/AttributeTables/sites_terrestrial.txt") # nearest terrestrial area info for each point
terrestrial_forests <- read.csv("data/DataSets_7_7/AttributeTables/Terrestrial_Forests.txt") # info about terrestrial areas/forests
sites_forest <- read.csv("data/DataSets_7_7/AttributeTables/sites_forest.txt") # nearest forest info for each point
lc_2010 <- read.csv("data/DataSets_7_7/AttributeTables/lc_2010.txt") # landcover info

# Add a `year` column to fish_data_EF
fish_data_EF$year <- str_extract(as.character(fish_data_EF$sdate), pattern = "[:digit:]+$")

# Join river miles to the sites_p4p8p13 data
# Join the river mile data
sites_p4p8p13 <- left_join(sites_p4p8p13, rivmi[,c("RIVER_MILE", "TARGET_FID")], by = c("FID" = "TARGET_FID"))
   # We're only going to use sites_p4p8p13 for the river mile data; all other columns are already present in sites_aa_5m. 

# Join barcodes from `fish_data_EF` to `sites_p4p8p13` (the reprojected data)
rows <- sites_p4p8p13$Field1 # `Field 1` in this dataset corresonds to the row number in fish_data_EF.
sites_p4p8p13$barcode <- fish_data_EF$barcode[rows] # Select the barcodes we want and add them as a column to sites_p4p8p13
   # This will allow for a join with sites_aa_5m. 

# Join barcodes and pools for sites_aa_5m
sites_aa_5m$barcode <- fish_data_EF$barcode[rows]
sites_aa_5m$pool <- fish_data_EF$pool[rows]
    
#reorder the columns so that barcode is first                                      
sites_aa_5m <- sites_aa_5m[,c(1:4, 71, 5:70)]   

# join river miles
sites_aa_5m <- dplyr::left_join(sites_aa_5m, sites_p4p8p13[,c("barcode", "RIVER_MILE")], by = "barcode")
  # Ok, now we're done with sites_p4p8p13

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer". I'd like these to have values of NA, not 0. 
badrows_5 <- sites_aa_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
dim(badrows_5)

# Remove the bad rows
sites_aa_5m <- sites_aa_5m %>% filter(Field1 %notin% badrows_5$Field1)
dim(sites_aa_5m)

# Where do we have missing values?
locate.nas(sites_aa_5m)

#there are a concerning number of NA's in the `pool` column that shouldn't be there. Luckily, the `uniq_id` column tells us which pool these are from. 
addpools <- function(df){
  pools <- as.numeric(substr(x = as.character(df$uniq_id), 
                             start = 2, 
                             stop = 3))
  df$pool[is.na(df$pool)] <- pools[is.na(df$pool)]
  return(df)
}
sites_aa_5m <- addpools(sites_aa_5m)
locate.nas(sites_aa_5m)

# Add columns for *distance* to terrestrial areas
  rows_5 <- sites_aa_5m$Field1 # `Field 1` in this dataset corresponds to the FID in the terrestrial areas dataset. 
  sites_aa_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_5] # which terrestrial region is the closest?
  sites_aa_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_5] # how far is that terrestrial region?

# Add information about nearest terrestrial areas
  # We're going to pull terrestrial areas information columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
  columns_terr <- lc_2010[, c("FID", "CLASS_7_C", "CLASS_7_N")] # define which columns we want (class 7 names and codes)
  sites_aa_5m <- left_join(sites_aa_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID")) # join the columns

  #change column names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
  sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_TERR_CLASS_7 = CLASS_7_C,
                                         NEAR_TERR_CLASS_7_N = CLASS_7_N)

# Add columns for *distance* to nearest forested area
sites_aa_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_5] # which forested region is the closest?
sites_aa_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_5] # how far is that forested region?
 
# Add information about nearest forested areas: 
  # As long as we're only using the 7-class forest divisions, we don't need to add information, since they all have the same type.
  table(terrestrial_forests$CLASS_7_N)

# Check for NA's
locate.nas(sites_aa_5m)
  # we have a bunch of NA's for snag, and 7 for river mile. 

# Join relevant point-level fish survey data to the data frame.
tojoin_5 <- fish_data_EF %>% dplyr::select(year, sitetype, gear, wingdyke, riprap, trib, barcode) %>% unique() %>% filter(barcode %in% sites_aa_5m$barcode)
sites_aa_5m <- left_join(sites_aa_5m, tojoin_5, by = "barcode")

# Exclude values with a sitetype of 2 (fixed sites) and exclude fyke net sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(sitetype != 2) # remove fixed sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(gear == "D") # remove all but daytime electrofishing data
dim(sites_aa_5m)

# Remove one point that has NEAR_TERR_CLASS.p equal to agriculture
table(sites_aa_5m$NEAR_TERR_CLASS_7_N)
sites_aa_5m <- sites_aa_5m %>% filter(NEAR_TERR_CLASS_7 != "Ag")

# Check that we've excluded any data before 1993
table(sites_aa_5m$year, exclude = NULL) # looks good

#=========================================
# Reducing the variables
#=========================================
# Exclude variables that we aren't going to use
names(sites_aa_5m)
excl1 <- c("OBJECTID", "aa_num", "AQUA_CODE", "AQUA_DESC", "Area", "Acres", "Hectares", "bath_pct", "sd_depth", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "avg_fetch", "econ", "sill", "min_rm", "max_rm", "len_met", "len_outl", "pct_outl", "num_outl", "len_oute", "pct_oute", "num_oute", "pct_aqveg", "pct_opwat", "len_terr", "pct_chan", "len_wetf", "len_wd", "wdl_p_m2", "num_wd", "sco_wd", "psco_wd", "len_revln", "rev_p_m2", "num_rev", "pct_rev", "pct_rev2", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "sinuosity", "year_phot", "NEAR_TERR_FID", "NEAR_FOREST_FID", "sitetype", "gear", "Join_Count", "FID", "TARGET_FID", "Field1", "pct2wetf", "trib")
all_reduced <- sites_aa_5m %>% dplyr::select(-excl1)
names(all_reduced)

# Change some names
all_reduced <- all_reduced %>% rename(perimeter = Perimeter,
                                      shoreline_density_index = sdi,
                                      pct_prm_wetf = pct1wetf,
                                      river_mile = RIVER_MILE,
                                      near_terr_dist = NEAR_TERR_DIST,
                                      near_terr_class_7 = NEAR_TERR_CLASS_7,
                                      near_terr_name = NEAR_TERR_CLASS_7_N,
                                      near_forest_dist = NEAR_FOREST_DIST)

# Make any -99's into NA's. Start at the second column to avoid the `barcode` column.
for(i in 2:ncol(all_reduced)){
  all_reduced[,i][all_reduced[,i] < -5000] <- NA
}

locate.nas(all_reduced)
arc <- as.data.table(all_reduced)
arc2 <- na.omit(object = arc, cols = c("snag", "max_depth", "avg_depth", "tot_vol", "wingdyke", "riprap"))
arc3 <- as.data.frame(arc2)

# Rename as all_reduced_clean
all_reduced_clean <- arc3
locate.nas(all_reduced_clean) # find the NA's: we still have a few for river mile, but they can stay. 
dim(all_reduced_clean) # check that dimensions match the consort diagram: should have 5439 observations at this point.

# Change data types of columns as needed
all_reduced_clean$pool <- factor(as.character(all_reduced_clean$pool)) 
all_reduced_clean$year <- as.numeric(all_reduced_clean$year)

# Exclude UXO
all_reduced_clean <- all_reduced_clean %>% filter(stratum != "UXO")
dim(all_reduced_clean)

# Export dataset
save(all_reduced_clean, file = "data/all_reduced_clean.Rda")

