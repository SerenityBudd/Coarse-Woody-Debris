source("libraries.R")

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
#See Molly's email 6/22/18 at 2pm for descriptions of variables. Also see the metadata file: https://www.sciencebase.gov/catalog/file/get/5a708ef0e4b06e28e9cae58f?f=__disk__26%2F00%2F43%2F260043b3d8895c99f3be0a19f9f6816214bd35e6&transform=1&allowOpen=true
source("libraries.R")

new.ef <- read.csv("data/p8_lwd_electro_jj(electrofishingdata).txt")
#See document "HNA II Aquatic Areas appendix A" in drive folder for description of the variables.  
#name variables more descriptively

names(new.ef)[25:27] <- c("landcover_abbr", # CLASS_31
                          "landcover_short", # CLASS_31_N
                          "landcover_desc")  # CLASS_31_D
names(new.ef)[29] <- "dist_landcover" # NEARDIST_T
names(new.ef)[31] <- "dist_aquahab" # NEAR_DIST
names(new.ef)[34:35] <- c("aqua_code", # AQUA_CODE
                          "aqua_desc") # AQUA_DESC
new.ef$snagyn <- ifelse(new.ef$snag == 0, "no", "yes") # new column based on `snag`
names(new.ef)[49] <- "shoreline_density_index" # sdi
names(new.ef)[67] <- "pct_prm_wetf" # pct1wetf
names(new.ef)[68] <- "pct_terr_shore_wetf" # pct2wetf
names(new.ef)[55] <- "len_prm_lotic" # len_outl
names(new.ef)[56] <- "pct_prm_lotic" # pct_outl
names(new.ef)[57] <- "num_lotic_outl" # num_outl
names(new.ef)[58] <- "len_prm_lentic" # len_oute
names(new.ef)[59] <- "pct_prm_lentic" # pct_oute
names(new.ef)[60] <- "num_lentic_outl" # num_oute
names(new.ef)[65] <- "pct_aq" # pct_chan
names(new.ef)[72] <- "scour_wd" # sco_wd
names(new.ef)[77] <- "pct_terr_shore_rev" # pct_rev
names(new.ef)[78] <- "pct_prm_rev" # pct_rev2

#create column with stratum names
new.ef$stratum_name[new.ef$stratum == "SCB"] <- "Side Channel Border"
new.ef$stratum_name[new.ef$stratum == "MCB-U"] <- "Main Channel Border--Unstructured"
new.ef$stratum_name[new.ef$stratum == "MCB-W"] <- "Main Channel Border--Wing Dam Area"
new.ef$stratum_name[new.ef$stratum == "TWZ"] <- "Tailwater Zone"
new.ef$stratum_name[new.ef$lstratum == "BWC-S"] <- "Backwater, Contiguous Shoreline"
new.ef$stratum_name[new.ef$stratum == "IMP-O"] <- "Impounded--Offshore"
new.ef$stratum_name[new.ef$stratum == "IMP-S"] <- "Impounded--Shoreline"
new.ef$stratum_name <- factor(new.ef$stratum_name)

#project the utm easting and northing onto a CRS using utm zone 15
new.ef_sp <- SpatialPoints(new.ef[,c("utm_e", "utm_n")], proj4string = CRS("+proj=utm +zone=15 +datum=WGS84"))

#transform to latlon, save as a data frame
new.ef_lonlat <- as.data.frame(spTransform(new.ef_sp, CRS("+proj=longlat +datum=WGS84")))

#rename the columns
names(new.ef_lonlat) <- c("lon", "lat")
#join to the original data frame
new.ef <- cbind(new.ef, new.ef_lonlat)

#split nearest landcover into fewer categories
new.ef$landcover_lumped <- NA
new.ef$landcover_lumped[new.ef$landcover_short %in% c("Lowland forest", "Salix community", "Upland forest", "Floodplain forest", "Populus community")] <- "Forest"
new.ef$landcover_lumped[new.ef$landcover_short %in% c("Deep marsh annual", "Shallow marsh perennial", "Deep marsh perennial", "Rooted floating aquatics", "Submersed aquatic vegetation")] <- "Aquatic veg"
new.ef$landcover_lumped[new.ef$landcover_short %in% c("Developed", "Roadside", "Levee")] <- "Developed"
new.ef$landcover_lumped[new.ef$landcover_short %in% c("Grassland", "Wet meadow shrub", "Wet meadow")] <- "Grassland or meadow"
new.ef$landcover_lumped[new.ef$landcover_short %in% c("Sand")] <- "Sand"
new.ef$landcover_lumped <- factor(new.ef$landcover_lumped)
levels(new.ef$landcover_lumped)
sum(is.na(new.ef$landcover_lumped))
table(new.ef$landcover_lumped)

save(new.ef, file = "data/new.ef.Rda")




head(new.ef)
head(pool8.barcodes)
sum(new.ef$barcode %in% pool8.barcodes$barcode)/nrow(new.ef)
identical(sort(unique(pool8.barcodes$barcode)), sort(unique(new.ef$barcode)))
  # we have all of the same barcodes. Good. 
strata <- new.ef[,c("barcode", "stratum_name", "stratum")]
#save(strata, file = "data/strata.Rda")
pool8.barcodes <- left_join(pool8.barcodes, strata, by = "barcode")
#save(pool8.barcodes, file = "data/pool8.barcodes.Rda")


