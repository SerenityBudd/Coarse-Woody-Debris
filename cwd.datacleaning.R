source("libraries.R")

fishdat <- read.csv("ltrm_fish_data.csv")
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

#save(pool8.barcodes, file = "pool8.barcodes.Rda")
for (i in 1:ncol(pool8.barcodes)) {
  print(colnames(pool8.barcodes)[i]) 
  print(summary(pool8.barcodes[,i]))
}

#########
#See Molly's email 6/22/18 at 2pm for descriptions of variables. Also see the metadata file: https://www.sciencebase.gov/catalog/file/get/5a708ef0e4b06e28e9cae58f?f=__disk__26%2F00%2F43%2F260043b3d8895c99f3be0a19f9f6816214bd35e6&transform=1&allowOpen=true
source("libraries.R")

newdat <- read.csv("more.electrofishing.data.txt")
names(newdat)[25:27] <- c("landcover_abbr", "landcover_short", "landcover_desc")
names(newdat)[29] <- "dist_landcover"
names(newdat)[31] <- "dist_aquahab"
names(newdat)[34:35] <- c("aqua_code", "aqua_desc")
newdat$snagyn <- ifelse(newdat$snag == 0, "no", "yes")
with(newdat, table(snagyn, stratum))
with(newdat, table(snagyn, stratum, year))
newdat$stratum_name[newdat$stratum == "SCB"] <- "Side Channel Border"
newdat$stratum_name[newdat$stratum == "MCB-U"] <- "Main Channel Border--Unstructured"
newdat$stratum_name[newdat$stratum == "MCB-W"] <- "Main Channel Border--Wing Dam Area"
newdat$stratum_name[newdat$stratum == "TWZ"] <- "Tailwater Zone"
newdat$stratum_name[newdat$stratum == "BWC-S"] <- "Backwater, Contiguous Shoreline"
newdat$stratum_name[newdat$stratum == "IMP-O"] <- "Impounded--Offshore"
newdat$stratum_name[newdat$stratum == "IMP-S"] <- "Impounded--Shoreline"
newdat$stratum_name <- factor(newdat$stratum_name)

newdat %>% group_by(year, stratum) %>% summarize(propsnag = sum(snag == 1)/sum(snag %in% c(0,1)))

#project the utm easting and northing onto a CRS using utm zone 15
newdat_sp <- SpatialPoints(newdat[,c("utm_e", "utm_n")], proj4string = CRS("+proj=utm +zone=15 +datum=WGS84"))

#transform to latlon, save as a data frame
newdat_lonlat <- as.data.frame(spTransform(newdat_sp, CRS("+proj=longlat +datum=WGS84")))

#rename the columns
names(newdat_lonlat) <- c("lon", "lat")
#join to the original data frame
newdat <- cbind(newdat, newdat_lonlat)
#save(newdat, file = "newdat.Rda")


