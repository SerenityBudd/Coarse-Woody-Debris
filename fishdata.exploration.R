fishdat <- read.csv("fishdata/ltrm_electro_fish_data_alloptions/ltrm_fish_data.csv")
head(fishdat)
str(fishdat)
library(plyr)
library(dplyr)
# Filter out the rows from pool8 and create the pool 8 dataframe
pool8 <- fishdat %>%
  filter(pool == "08") %>% droplevels()

# Take a look at the data frame
head(pool8, 3)

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
library(rgdal)
library(sp)
#project the utm easting and northing onto a CRS using utm zone 15
sputm <- SpatialPoints(pool8[,c("utm_e", "utm_n")], proj4string = CRS("+proj=utm +zone=15 +datum=WGS84"))
#transform to latlon, save as a data frame
lonlat <- as.data.frame(spTransform(sputm, CRS("+proj=longlat +datum=WGS84")))
#rename the columns
names(lonlat) <- c("lon", "lat")
#join to the original data frame
pool8 <- cbind(pool8, lonlat)
pool8$snag <- factor(pool8$snag)

# How many sites are there?
length(unique(pool8$site))
# 65 unique sites
# How many times was each site visited? Aka how many barcodes per site?
library(dplyr)
newdf <- data.frame(site = unique(pool8$site))
nb <- function(x){
  temp <- pool8[pool8$site == x,]
  return(length(unique(temp$barcode)))
}
newdf$nbarcodes <- sapply(newdf$site, FUN = nb)
newdf <- newdf[order(newdf$nbarcodes),]
newdf
range(newdf$nbarcodes)

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
head(pool8.barcodes, 3)
tail(pool8.barcodes, 3)

   # remove rows that are entirely NA
ind <- apply(pool8.barcodes, 1, function(x) all(is.na(x)))
  # there don't seem to be any
write.csv(pool8.barcodes, "pool8.barcodes.csv", row.names = F)

## MAPPING
# POOL 8 SNAG PRESENCE/ABSENCE BY BARCODE
library(ggmap)

#get map of Pool 8
m <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), zoom = 11, maptype = "terrain", source = "google")
ggmap(m)

#plot map with points on it 
pool8.barcodes.snagmap <- ggmap(m)+
  ggtitle("Pool 8 Sampling Locations by CWD Presence")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
             aes(x = lon, y = lat, color = snag), size = 0.4, alpha = 0.8)+
  scale_color_manual(values=c("#600000", "#ff0000"))+
  guides(color = guide_legend(override.aes = list(size=8)))
pool8.barcodes.snagmap
#ggsave(filename = "pool8.barcodes.snagmap.png", plot = pool8.barcodes.snagmap, dpi = 1000)
  # Used the code to remove rows where snag was NA. ggmap would have removed them automatically but would have left a factor level for them in the legend, which we don't want.
  # How do we make the legend a little bigger?

# Dealing with location codes
ldat <- read.csv("locationdata.csv")
dim(ldat)
ldat <- ldat[ldat$Location.code %in% pool8.barcodes$lcode,] 

# This website (https://umesc.usgs.gov/data_library/fisheries/fish_tables.html#starting_year) gives valid UTM northing and easting ranges for Pool 8.   # valid UTM northing range: 4825460-4861343
  # valid UTM easting range: 635661-644991
vn <- c(4825460, 4861343)
ve <- c(635661, 644991)
rn <- range(pool8.barcodes$utm_n)
re <- range(pool8.barcodes$utm_e)

vn[2]-vn[1] >= rn[2]-rn[1]
ve[2]-ve[1] >= re[2]-re[1]
  # both are true, so all of our UTM values fall within valid UTM range boundaries. 

# POOL 8 LOCATION CODES

# If all barcodes are taken from a location, then why are more/different points shown in the barcodes map than in the location codes map? Shouldn't they be the same, but with some points overlapping in the barcodes map?
loc <- pool8.barcodes[,names(pool8.barcodes) %in% c("barcode", "site", "fstation", "sitetype", "pool", "utmzone", "utm_e", "utm_n", "gisgrid", "lon", "lat", "lcode")]

# plot only the fixed sites
pool8.barcodes[pool8.barcodes$sitetype == "subj.perm",]
a <- pool8.barcodes[pool8.barcodes$sitetype == "subj.perm" & !is.na(pool8.barcodes$sitetype), ]
a <- droplevels(a)
table(a$lcode) #there are literally only two location codes that are fixed.
table(a$barcode) #but there are a bunch of barcodes, which makes sense if each of the locations was visited multiple times. 
    #this shows lots and lots of rows that are entirely NA
ind <- apply(pool8.barcodes, 1, function(x) all(is.na(x)))
sum(ind)
    #this shows zero rows that are entirely NA

#  plot fixed sites only 
pool8.fixedsites <- ggmap(m)+
  ggtitle("Pool 8 Fixed Sites")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[pool8.barcodes$sitetype == "subj.perm" & !is.na(pool8.barcodes$sitetype), ], aes(x = lon, y = lat), size = 1, alpha = 0.8, color = "red")
pool8.fixedsites

# check across all the data, not just pool8
b <- droplevels(fishdat[fishdat$sitetype == "2" & !is.na(fishdat$sitetype),])
table(b$lcode)
# sure enough, there are more permanent sites in the other pools, just not pool 8.

# make another plot color-coded by site type
#get map of Pool 8
m <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), zoom = 11, maptype = "terrain", source = "google")
ggmap(m)

#plot map with points on it 
pool8.barcodes.sitetype <- ggmap(m)+
  ggtitle("Pool 8 Barcodes by Site Type")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$sitetype),], 
             aes(x = lon, y = lat, color = sitetype), size = 0.3, alpha = 0.6, pch = 20)+
  scale_color_manual(name = "Site Type", labels = c("Primary Random", "Alternate Random", "Subjective Permanent"), values=c("#7302F8", "#F87302", "#02F873"))+
  guides(color = guide_legend(override.aes = list(size=10)))
pool8.barcodes.sitetype
#ggsave(filename = "pool8.barcodes.sitetype.png", plot = pool8.barcodes.sitetype, dpi = 1000)



###########
# Some plots of the different barcodes to see relationships between snags and other things

#Boxplot of current by CWD
pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=current, fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Current by CWD Presence")+
  xlab("Coarse Woody Debris Presence")+
  ylab("Current")+
  guides(fill = FALSE)

#Boxplot of log(total fish count) by CWD
logfct.cwd <- pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=log(totfishc), fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Log of Fish Count by CWD Presence")+
  xlab("Coarse Woody Debris Presence")+
  ylab("Log of Total Number of Fish Caught")+
  guides(fill = FALSE)
ggsave(filename = "logfct.cwd.png", plot = logfct.cwd, dpi = 500)

  # Make a vector of labels for the site types
  labels <- c(prim.rand = "Primary Random Site", alt.rand = "Alternate Random Site", subj.perm = "Subjective Permanent Site")
# Boxplot of log(total fish count) by CWD, split by site type
logfct.cwd.facet <- pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=log(totfishc), fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Log of Fish Count by CWD Presence")+
  xlab("Coarse woody debris presence, split by site type")+
  ylab("Log of Total Number of Fish Caught")+
  facet_wrap(~sitetype, labeller = labeller(sitetype = labels))+
  guides(fill = FALSE)
ggsave(filename = "logfct.cwd.facet.png", plot = logfct.cwd.facet, dpi = 500)

# have this suggestion from here for a loess plot: https://stats.stackexchange.com/questions/45444/how-do-you-visualize-binary-outcomes-versus-a-continuous-predictor
loess <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = lat, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.2, pch = 20)+
  stat_smooth(method = "loess", color = "darkred", size = 1.5) +
  xlab("N <--    (Latitude)    --> S") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 CWD Presence by Latitude")
ggsave(filename = "loess.latitude.png", plot = loess, dpi = 500)
  # can also use scatter.smooth
  # with(pool8.barcodes, scatter.smooth(snag~lat))

# loess plot over time (by date)
ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = sdate, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.3, pch = 20)+
  stat_smooth(method = "loess", color = "blue", size = 1) +
  xlab("Date") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 snag presence by date")+
  theme_bw()

substrt.cwd <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$substrt),], aes(x = substrt))+
  geom_bar(aes(fill = substrt))+
  scale_fill_manual(name = "Substrate", labels = c("Silt", "Silt/Clay/Little Sand", "Sand/Mostly Sand", "Gravel/Rock/Hard Clay"), values=wes_palette("Darjeeling1"))+
  facet_wrap(~snag)+
  ylab("Number of Sampling Events")+
  xlab("Substrate Type")+
  ggtitle("Substrate Distributions by CWD Presence")+
  theme(axis.text.x = element_blank())
ggsave(filename = "substrt.cwd.png", plot = substrt.cwd, dpi = 500)
  

## Logistic regression for predicting CWD
library(car)

#Turbidity
pool8.barcodes$snag <- as.numeric(as.character(pool8.barcodes$snag))
msecchi <- glm(snag~secchi, 
               data = pool8.barcodes[!is.na(pool8.barcodes$snag) & is.na(pool8.barcodes$s_qf),], 
               family = binomial)
summary(msecchi)
mmps(msecchi, main = "Marginal Model Plots for Turbidity Measurements")

secchi <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & is.na(pool8.barcodes$s_qf),], 
                 aes(x = secchi, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "dodgerblue2")+
  ggtitle("CWD Presence vs. Water Clarity")+
  xlab("more turbidity <--     (Secchi disk measurement)     --> less turbidity")+
  ylab("CWD Presence/Absence")+
  theme(plot.title = element_text(size = 18), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
secchi
ggsave(filename = "secchi.png", plot = secchi, dpi = 500)
  

#Depth
mdepth <- glm(snag~depth, data = pool8.barcodes, family = binomial)
summary(mdepth)
mmps(mdepth)

ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$depth),], 
       aes(x = depth, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Depth")

#Temperature
mtemp <- glm(snag~temp, data = pool8.barcodes, family = binomial)
summary(mtemp)
mmps(mtemp)

ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$temp),], 
       aes(x = temp, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Temperature")

#FISH COUNTS: yikes
#~~~~~~~~~~~~~~~~~~~~
# Make a matrix that shows the number of individuals of each species captured at each barcode
fishsp.perbarcode <- with(pool8, table(barcode,fishcode)) %>% as.data.frame.matrix()
fishsp.perbarcode$barcode <- rownames(fishsp.perbarcode)
rownames(fishsp.perbarcode) <- NULL
fishsp.perbarcode <- fishsp.perbarcode[,c(103, 1:102)]
fishsp.perbarcode$barcode <- as.integer(fishsp.perbarcode$barcode)




