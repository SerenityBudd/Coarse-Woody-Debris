source("libraries.R")
source("ownfunctions.R")

fish_sites_EF <- read.csv("data/DataSets_7_7/ltrm_fish_sites_EF.csv")
fish_data_EF <- read.csv("data/DataSets_7_7/ltrm_fish_data_EF.csv")
aqa_2010_lvl3 <- read.csv("data/DataSets_7_7/AttributeTables/aqa_2010_lvl3_011918.txt")
aquatic <- read.csv("data/DataSets_7_7/AttributeTables/Aquatic.txt")
lc_2010 <- read.csv("data/DataSets_7_7/AttributeTables/lc_2010.txt")
sites_aquaareas <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas.txt")
sites_aquaareas_5m <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas5m.txt")
sites_forest <- read.csv("data/DataSets_7_7/AttributeTables/sites_forest.txt")
sites_p4p8p13 <- read.csv("data/DataSets_7_7/AttributeTables/sites_p4p8p13.txt")
sites_terrestrial <- read.csv("data/DataSets_7_7/AttributeTables/sites_terrestrial.txt")
terrestrial_forests <- read.csv("data/DataSets_7_7/AttributeTables/Terrestrial_Forests.txt")
terrestrial <- read.csv("data/DataSets_7_7/AttributeTables/Terrestrial.txt")

# Step 1. Join barcodes and pool# from `fish_data_EF` to `fish_sites_EF`
rows <- fish_sites_EF$RowID
barcodes <- fish_data_EF$barcode[rows]
pools <- fish_data_EF$pool[rows]
fish_sites_EF$barcode <- barcodes
fish_sites_EF$pool <- pools

# Step 2. Do the same thing for the reprojected data: sites_p4p8p13
rows2 <- sites_p4p8p13$Field1
barcodes2 <- fish_data_EF$barcode[rows2]
pools2 <- fish_data_EF$pool[rows2]
    #just to verify:
    identical(barcodes, barcodes2) #TRUE
sites_p4p8p13$barcode <- barcodes2
sites_p4p8p13$pool <- pools2
    #check that we only have pools 4, 8, and 13
    table(sites_p4p8p13$pool)
    #good. 

# Step 3. Also join barcodes for sites_aquaareas and sites_aquaareas_5m
sites_aquaareas$barcode <- barcodes2
sites_aquaareas$pool <- pools2
sites_aquaareas_5m$barcode <- barcodes2
sites_aquaareas_5m$pool <- pools2
  #reorder the columns so that barcode is first
sites_aquaareas <- sites_aquaareas[,c(1:4, 71, 5:70)]
sites_aquaareas_5m <- sites_aquaareas_5m[,c(1:4, 71, 5:70)]

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer"
# I'd like these to have values of NA, not 0. 
# Figure out which variables come from the aqa_2010_lvl2 file
badrows_0 <- sites_aquaareas %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
badrows_5 <- sites_aquaareas_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
    dim(badrows_0)
    dim(badrows_5)
    #nice. we see a reduction in the number of bad rows once points are buffered by 5m, just like we expected. 
  
# Remove the bad rows
sites_aquaareas <- sites_aquaareas %>% filter(Field1 %notin% badrows_0$Field1)
sites_aquaareas_5m <- sites_aquaareas_5m %>% filter(Field1 %notin% badrows_5$Field1)
dim(sites_aquaareas)
dim(sites_aquaareas_5m)

# Add columns for distance to terrestrial areas
rows_0 <- sites_aquaareas$Field1
rows_5 <- sites_aquaareas_5m$Field1
sites_aquaareas$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_0]
sites_aquaareas$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_0]

sites_aquaareas_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_5]
sites_aquaareas_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_5]

# We're going to pull columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
# Which columns do we want to pull in? Need info on what these columns mean.
columns_terr <- lc_2010[, c("FID", "CLASS_31", "CLASS_31_N","CLASS_15_N", "CLASS_7_N", "HEIGHT_N")]
sites_aquaareas <- left_join(sites_aquaareas, columns_terr, by = c("NEAR_TERR_FID" = "FID"))
sites_aquaareas_5m <- left_join(sites_aquaareas_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID"))
  #change names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_31'] <- 'NEAR_TERR_CLASS_31'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_31_N'] <- 'NEAR_TERR_CLASS_31_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_15_N'] <- 'NEAR_TERR_CLASS_15_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_7_N'] <- 'NEAR_TERR_CLASS_7_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'HEIGHT_N'] <- 'NEAR_TERR_HEIGHT_N'

names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_31'] <- 'NEAR_TERR_CLASS_31'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_31_N'] <- 'NEAR_TERR_CLASS_31_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_15_N'] <- 'NEAR_TERR_CLASS_15_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_7_N'] <- 'NEAR_TERR_CLASS_7_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'HEIGHT_N'] <- 'NEAR_TERR_HEIGHT_N'
  
# Add columns for distance to nearest forested area
columns_forest <- terrestrial_forests[, c("FID", "CLASS_31", "CLASS_31_N", "CLASS_15_N", "CLASS_7_N", "HEIGHT_N")]
sites_aquaareas$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_0]
sites_aquaareas$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_0]

sites_aquaareas_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_5]
sites_aquaareas_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_5]

#join the attribute columns for the nearest *forested* area
sites_aquaareas <- left_join(sites_aquaareas, columns, by = c("NEAR_FOREST_FID" = "FID"))
sites_aquaareas_5m <- left_join(sites_aquaareas_5m, columns, by = c("NEAR_FOREST_FID" = "FID"))
  #change names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_31'] <- 'NEAR_FOREST_CLASS_31'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_31_N'] <- 'NEAR_FOREST_CLASS_31_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_15_N'] <- 'NEAR_FOREST_CLASS_15_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'CLASS_7_N'] <- 'NEAR_FOREST_CLASS_7_N'
names(sites_aquaareas)[names(sites_aquaareas) == 'HEIGHT_N'] <- 'NEAR_FOREST_HEIGHT_N'

names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_31'] <- 'NEAR_FOREST_CLASS_31'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_31_N'] <- 'NEAR_FOREST_CLASS_31_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_15_N'] <- 'NEAR_FOREST_CLASS_15_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'CLASS_7_N'] <- 'NEAR_FOREST_CLASS_7_N'
names(sites_aquaareas_5m)[names(sites_aquaareas_5m) == 'HEIGHT_N'] <- 'NEAR_FOREST_HEIGHT_N'

# Any NA's?
locate.nas(sites_aquaareas)
locate.nas(sites_aquaareas_5m)
  #looks good except for the large number of NA's for pool number. Not sure why this is. 

# What are the levels of terrestrial habitat types?
table(droplevels(sites_aquaareas$NEAR_TERR_CLASS_31_N)) #looks good, no water
table(droplevels(sites_aquaareas_5m$NEAR_TERR_CLASS_31_N)) #likewise. 

# Append point-level data to each data frame
pointcols <- c("depth", "current", "gear", "stageht", "substrt", "wingdyke")
pointcols_plus <- c(pointcols, "barcode")
sites_aquaareas <- left_join(sites_aquaareas, fish_data_EF[, pointcols_plus], by = "barcode")
sites_aquaareas_5m <- left_join(sites_aquaareas_5m, fish_data_EF[, pointcols_plus], by = "barcode")
for(i in 1:length(pointcols)){
  colnames(sites_aquaareas)[colnames(sites_aquaareas) == pointcols[i]] <- paste0(pointcols[i], "-p")
  colnames(sites_aquaareas_5m)[colnames(sites_aquaareas_5m) == pointcols[i]] <- paste0(pointcols[i], "-p")
}
 
#now move to the data cleaning document. 

