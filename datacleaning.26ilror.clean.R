# Data cleaning: LW; Pool 26, Illinois River and Open River
# Kaija Gahm
# 20 August 2018

# Source libraries and own functions
source("ownfunctions.R")
source("libraries.R")

# Load files
load("data/DataSets_8_17/ltrm_fish_data.Rda") # Fish sampling data
sites_p26ilror <- read.csv("data/DataSets_8_17/AttributeTables/sites_p26ilror.txt") # reprojected data
sites_aa_5m <- read.csv("data/DataSets_8_17/AttributeTables/sites_aquaareas5m2.txt") # merged aquatic areas data for points buffered by 5 meters.
sites_terrestrial <- read.csv("data/DataSets_8_17/AttributeTables/sites_terrestrial2.txt") # nearest terrestrial area info for each point
terrestrial_forests <- read.csv("data/DataSets_8_17/AttributeTables/Terrestrial_Forests2.txt") # info about terrestrial areas/forests
sites_forest <- read.csv("data/DataSets_8_17/AttributeTables/sites_forest2.txt") # nearest forest info for each point
lc_2010 <- read.csv("data/DataSets_8_17/AttributeTables/lc_20102.txt") # landcover info

# Add a `year` column to ltrm_fish_data
ltrm_fish_data$year <- str_extract(as.character(ltrm_fish_data$sdate), pattern = "[:digit:]+$")

# Join barcodes from `ltrm_fish_data` to `sites_p26ilror` (the reprojected data)
rows <- sites_p26ilror$RowID_ # `RowID_` in this dataset corresonds to the row number in ltrm_fish_data.
sites_p26ilror$barcode <- ltrm_fish_data$barcode[rows] # Select the barcodes we want and add them as a column to sites_p26ilror
# This will allow for a join with sites_aquaareas5m2. 

# Join barcodes and pools for sites_aa_5m
sites_aa_5m$barcode <- ltrm_fish_data$barcode[rows]
sites_aa_5m$pool <- ltrm_fish_data$pool[rows]

#reorder the columns so that barcode is first                                      
sites_aa_5m <- sites_aa_5m[,c(1:4, 72, 5:71)]   

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer". I'd like these to have values of NA, not 0. 
badrows_5 <- sites_aa_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
dim(badrows_5)

# Remove the bad rows
sites_aa_5m <- sites_aa_5m %>% filter(RowID_ %notin% badrows_5$RowID_)
dim(sites_aa_5m)

# Where do we have missing values?
locate.nas(sites_aa_5m) # a few missing values for snag, but otherwise we're golden!

# Skip the part for fixing missing values of `pool` because we don't have any 

# Add columns for *distance* to terrestrial areas
rows_5 <- sites_aa_5m$RowID_ # `RowID_` in this dataset corresponds to the FID in the terrestrial areas dataset. 
sites_aa_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$RowID_ %in% rows_5] # which terrestrial region is the closest?
sites_aa_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$RowID_ %in% rows_5] # how far is that terrestrial region?

# Add information about nearest terrestrial areas
# We're going to pull terrestrial areas information columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
columns_terr <- lc_2010[, c("FID", "CLASS_7_C", "CLASS_7_N")] # define which columns we want (class 7 names and codes)
sites_aa_5m <- left_join(sites_aa_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID")) # join the columns

#change column names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_TERR_CLASS_7 = CLASS_7_C,
                                             NEAR_TERR_CLASS_7_N = CLASS_7_N)

# Add columns for *distance* to nearest forested area
sites_aa_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$RowID_ %in% rows_5] # which forested region is the closest?
sites_aa_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$RowID_ %in% rows_5] # how far is that forested region?

# Add information about nearest forested areas: 
# As long as we're only using the 7-class forest divisions, we don't need to add information, since they all have the same type.
table(terrestrial_forests$CLASS_7_N)

# Check for NA's
locate.nas(sites_aa_5m)
  # still, our only NA's are in the `snag` column. Excellent.

# Join relevant point-level fish survey data to the data frame.
tojoin_5 <- ltrm_fish_data %>% dplyr::select(year, sitetype, gear, wingdyke, riprap, trib, barcode) %>% unique() %>% filter(barcode %in% sites_aa_5m$barcode)
sites_aa_5m <- left_join(sites_aa_5m, tojoin_5, by = "barcode")


# Exclude values with a sitetype of 2 (fixed sites) and exclude fyke net sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(sitetype != 2) # remove fixed sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(gear == "D") # remove all but daytime electrofishing data
dim(sites_aa_5m)

# What distribution of NEAR_TERR_CLASS_7 do we have?
table(sites_aa_5m$NEAR_TERR_CLASS_7_N)

# Check that we've excluded any data before 1993
table(sites_aa_5m$year, exclude = NULL) # looks good

#=========================================
# Reducing the variables
#=========================================
# Exclude variables that we aren't going to use
names(sites_aa_5m)
excl1 <- c("OBJECTID", "aa_num", "AQUA_CODE", "AQUA_DESC", "Area", "Acres", "Hectares", "bath_pct", "sd_depth", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "avg_fetch", "econ", "sill", "min_rm", "max_rm", "len_met", "len_outl", "pct_outl", "num_outl", "len_oute", "pct_oute", "num_oute", "pct_aqveg", "pct_opwat", "len_terr", "pct_chan", "len_wetf", "len_wd", "wdl_p_m2", "num_wd", "sco_wd", "psco_wd", "len_revln", "rev_p_m2", "num_rev", "pct_rev", "pct_rev2", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "sinuosity", "year_phot", "NEAR_TERR_FID", "NEAR_FOREST_FID", "sitetype", "gear", "Join_Count", "FID", "TARGET_FID", "RowID_", "pct2wetf", "trib")
all2_reduced <- sites_aa_5m %>% dplyr::select(-excl1)
names(all2_reduced)

# Change some names
all2_reduced <- all2_reduced %>% rename(perimeter = Perimeter,
                                      shoreline_density_index = sdi,
                                      pct_prm_wetf = pct1wetf,
                                      near_terr_dist = NEAR_TERR_DIST,
                                      near_terr_class_7 = NEAR_TERR_CLASS_7,
                                      near_terr_name = NEAR_TERR_CLASS_7_N,
                                      near_forest_dist = NEAR_FOREST_DIST)

# Make any -99's into NA's. Start at the second column to avoid the `barcode` column.
for(i in 2:ncol(all2_reduced)){
  all2_reduced[,i][all2_reduced[,i] < -5000] <- NA
}

locate.nas(all2_reduced)
a2r <- as.data.table(all2_reduced)
a2r2 <- na.omit(object = a2r, cols = c("snag", "max_depth", "avg_depth", "tot_vol", "wingdyke", "riprap"))
a2r3 <- as.data.frame(a2r2)
dim(a2r3)

# Rename as all_reduced_clean
all2_reduced_clean <- a2r3
locate.nas(all2_reduced_clean) # find the NA's
dim(all2_reduced_clean) # check that dimensions match the consort diagram

# Change data types of columns as needed
all2_reduced_clean$pool <- factor(as.character(all2_reduced_clean$pool)) 
all2_reduced_clean$year <- as.numeric(all2_reduced_clean$year)

# Export dataset
save(all2_reduced_clean, file = "data/all2_reduced_clean.Rda")
