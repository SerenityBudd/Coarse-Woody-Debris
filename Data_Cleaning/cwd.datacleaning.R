source("libraries.R")
source("ownfunctions.R")

fishdat <- read.csv("data/ltrm_fish_data.csv")
head(fishdat)
str(fishdat)

# Filter out the rows from pool8 and create the pool 8 dataframe
pool8 <- fishdat %>%
  filter(pool == "08") %>% droplevels()

# Edit site names to remove duplicates
pool8$site <- as.character(pool8$site) #convert to character so we can edit easily
pool8$site <- trimws(pool8$site) #trim white space
pool8$site <- toupper(pool8$site) #convert to uppercase
sort(unique(pool8$site)) #show the current levels
pool8$site[pool8$site == ""] <- NA # convert blanks to NA's
pool8$site[pool8$site == "BROWNSVUILLE"] <- "BROWNSVILLE"
pool8$site[pool8$site == "CONEY COMPLE"] <- "CONEY COMPLEX"
pool8$site[pool8$site == "CONEYCOMPLEX"] <- "CONEY COMPLEX"
pool8$site[pool8$site == "FRENCH SLOUG"] <- "FRENCH SLOUGH"
pool8$site[pool8$site == "FRENCHSLOUGH"] <- "FRENCH SLOUGH"
pool8$site[pool8$site == "HORSHOE"] <- "HORSESHOE"
pool8$site[pool8$site == "I90 BAY"] <- "I-90 BAY"
pool8$site[pool8$site == "LAWRENCE LK"] <- "LAWRENCE LAKE"
pool8$site[pool8$site == "LAWRENCELK"] <- "LAWRENCE LAKE"
pool8$site[pool8$site == "SHADY MAPLE"] <- "SHADY MAPLES"
pool8$site[pool8$site == "TARGET LK"] <- "TARGET LAKE"
pool8$site[pool8$site == "UPPERPOOL"] <- "UPPER POOL"
pool8$site <- factor(pool8$site) #convert back to factor
sort(unique(pool8$site)) #take a look at the new levels

#convert dates and times to character 
pool8$sdate <- as.character(pool8$sdate)
pool8$fdate <- as.character(pool8$fdate)
pool8$stime <- as.character(pool8$stime)
pool8$ftime <- as.character(pool8$ftime)

# change the date and time formats with `hm()` and `mdy()` from the package lubridate
#pool8$stim <- hm(pool8$stime)
#pool8$ftim <- hm(pool8$ftime)

pool8$sdat <- chron(pool8$sdate, format = c(dates = "m/d/y"))
pool8$fdat <- chron(pool8$fdate, format = c(dates = "m/d/y"))

#change the coding for some of the variables
pool8$substrt <- recode_factor(pool8$substrt, `1` = "silt", `2` = "Silt/Clay/Little Sand", `3` = "Sand/Mostly Sand", `4` = "Gravel/Rock/Hard Clay")
pool8$sitetype <- recode_factor(pool8$sitetype, `0` = "prim.rand", `1` = "alt.rand", `2` = "subj.perm")

# Add lat/long columns

#project the utm easting and northing onto a CRS using utm zone 15
sputm <- SpatialPoints(pool8[,c("utm_e", "utm_n")], proj4string = CRS("+proj=utm +zone=15 +datum=WGS84"))
#transform to latlon, save as a data frame
lonlat <- as.data.frame(spTransform(sputm, CRS("+proj=longlat +datum=WGS84")))
#rename the columns
names(lonlat) <- c("lon", "lat")
#join to the original data frame
pool8 <- cbind(pool8, lonlat)
pool8$snag <- factor(pool8$snag)

# Assuming that data on CWD (and other environmental variables) were only taken once per sampling event (barcode), let's make a data frame with information per barcode.

# Function to extract values per barcode, or flag if this isn't possible. This function doesn't work and I don't know why. 
flag <- function(x){
  if(length(unique(x)) == 1){
    return(x[1])
  }
  else{
    print(9999999999)
  }
}
  #this still doesn't quite work right

# New function that just takes the first element, regardless.
firstel <- function(x){
  return(x[1])
}

# make a new working data set with a smaller subset of the columns in the original data. 
pool8.wrk <- pool8[ , !names(pool8) %in% c("batchno", "orphflag", "recorder", "userdef", "subproj", "pathcode", "weight", "catch", "grp_width", "tfs", "length", "fishcode", "rownum", "rec_site", "pageno", "leader", "contanrs", "labind")]

# Make a data frame with data summarized by barcode, using the flag function to extract a value for each barcode for each variable. 
pool8.barcodes <- as.data.frame(pool8 %>% 
                                  group_by(barcode) %>% 
                                  summarize_all(firstel))

#save(pool8.barcodes, file = "data/pool8.barcodes.Rda")
for (i in 1:ncol(pool8.barcodes)) {
  print(colnames(pool8.barcodes)[i]) 
  print(summary(pool8.barcodes[,i]))
}

############################
#New reprojected electrofishing and fyke net dataset from Molly.
source("ownfunctions.R")
source("libraries.R")
fish_data_EF <- read.csv("data/DataSets_7_7/ltrm_fish_data_EF.csv")
aqa_2010_lvl3 <- read.csv("data/DataSets_7_7/AttributeTables/aqa_2010_lvl3_011918.txt")
lc_2010 <- read.csv("data/DataSets_7_7/AttributeTables/lc_2010.txt")
sites_aquaareas <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas.txt")
sites_aquaareas_5m <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas5m.txt")
sites_forest <- read.csv("data/DataSets_7_7/AttributeTables/sites_forest.txt")
sites_p4p8p13 <- read.csv("data/DataSets_7_7/AttributeTables/sites_p4p8p13.txt")
sites_terrestrial <- read.csv("data/DataSets_7_7/AttributeTables/sites_terrestrial.txt")
terrestrial_forests <- read.csv("data/DataSets_7_7/AttributeTables/Terrestrial_Forests.txt")

# Join barcodes and pool # from `fish_data_EF` to `sites_p4p8p13` (the reprojected data)
rows <- sites_p4p8p13$Field1
barcodes <- fish_data_EF$barcode[rows]
pools <- fish_data_EF$pool[rows]
sites_p4p8p13$barcode <- barcodes
sites_p4p8p13$pool <- pools
#check that we only have pools 4, 8, and 13
table(sites_p4p8p13$pool)
#good. 

# Also join barcodes for sites_aquaareas and sites_aquaareas_5m
sites_aquaareas$barcode <- barcodes
sites_aquaareas$pool <- pools
sites_aquaareas_5m$barcode <- barcodes
sites_aquaareas_5m$pool <- pools
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

#create a "snagyn" column
sites_aquaareas$snagyn <- ifelse(sites_aquaareas$snag == 0, "no", "yes") # new column based on `snag`

#change some of the column names
names(sites_aquaareas)[49] <- "shoreline_density_index" # sdi
names(sites_aquaareas)[67] <- "pct_prm_wetf" # pct1wetf
names(sites_aquaareas)[68] <- "pct_terr_shore_wetf" # pct2wetf
names(sites_aquaareas)[55] <- "len_prm_lotic" # len_outl
names(sites_aquaareas)[56] <- "pct_prm_lotic" # pct_outl
names(sites_aquaareas)[57] <- "num_lotic_outl" # num_outl
names(sites_aquaareas)[58] <- "len_prm_lentic" # len_oute
names(sites_aquaareas)[59] <- "pct_prm_lentic" # pct_oute
names(sites_aquaareas)[60] <- "num_lentic_outl" # num_oute
names(sites_aquaareas)[65] <- "pct_aq" # pct_chan
names(sites_aquaareas)[72] <- "scour_wd" # sco_wd
names(sites_aquaareas)[77] <- "pct_terr_shore_rev" # pct_rev
names(sites_aquaareas)[78] <- "pct_prm_rev" # pct_rev2

#create reverse of area_gt* columns
#how much of the polygon is less than or equal to a certain depth? (cm)
sites_aquaareas$area_le50 <- sites_aquaareas$Area - sites_aquaareas$area_gt50
sites_aquaareas$area_le100 <- sites_aquaareas$Area - sites_aquaareas$area_gt100
sites_aquaareas$area_le200 <- sites_aquaareas$Area - sites_aquaareas$area_gt200
sites_aquaareas$area_le300 <- sites_aquaareas$Area - sites_aquaareas$area_gt300
sites_aquaareas$pct_area_le100 <- sites_aquaareas$area_le100/sites_aquaareas$Area
sites_aquaareas$pct_area_le50 <- sites_aquaareas$area_le50/sites_aquaareas$Area
sites_aquaareas$pct_area_le200 <- sites_aquaareas$area_le200/sites_aquaareas$Area
sites_aquaareas$pct_area_le300 <- sites_aquaareas$area_le300/sites_aquaareas$Area

#create column with stratum names
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "SCB"] <- "Side Channel Border"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "MCB-U"] <- "Main Channel Border--Unstructured"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "MCB-W"] <- "Main Channel Border--Wing Dam Area"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "TWZ"] <- "Tailwater Zone"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "BWC-S"] <- "Backwater, Contiguous Shoreline"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "IMP-O"] <- "Impounded--Offshore"
sites_aquaareas$stratum_name[sites_aquaareas$stratum == "IMP-S"] <- "Impounded--Shoreline"
sites_aquaareas$stratum_name <- factor(sites_aquaareas$stratum_name)

#remove weird slightly negative values of sinuosity
sites_aquaareas$sinuosity[sites_aquaareas$sinuosity == -9999] <- NA
sites_aquaareas$sinuosity[sites_aquaareas$sinuosity<0 & sites_aquaareas$sinuosity > -9999] <- 0

#Make any -99's into NA's. Start at 4 to avoid the barcodes.
for(i in 18:ncol(sites_aquaareas)){
  sites_aquaareas[,i][sites_aquaareas[,i] < -5000] <- NA
}

#save(sites_aquaareas, file = "data/sites_aquaareas.Rda")


#get stratum information from new.ef into pool8.barcodes
head(new.ef)
head(pool8.barcodes)
sum(new.ef$barcode %in% pool8.barcodes$barcode)/nrow(new.ef)
identical(sort(unique(pool8.barcodes$barcode)), sort(unique(new.ef$barcode)))
  # we have all of the same barcodes. Good. 
strata <- new.ef[,c("barcode", "stratum_name", "stratum")]
#save(strata, file = "data/strata.Rda")
pool8.barcodes <- left_join(pool8.barcodes, strata, by = "barcode")
pool8.barcodes$snagyn <- ifelse(pool8.barcodes$snag == 1, "yes", "no")
pool8.barcodes$snagyn <- factor(as.character(pool8.barcodes$snagyn))
#save(pool8.barcodes, file = "data/pool8.barcodes.Rda")

