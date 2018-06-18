## MAPPING
# POOL 8 SNAG PRESENCE/ABSENCE BY BARCODE
library(ggmap)
library(wesanderson)
library(ggplot2)

#read in the pool8 barcodes data.
pool8.barcodes <- read.csv("pool8.barcodes.csv")
str(pool8.barcodes)

# `snag` is an integer; let's change it to a factor
pool8.barcodes$snag <- factor(pool8.barcodes$snag)

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
             aes(x = lon, y = lat, color = snag), 
             size = 0.4, 
             alpha = 0.8)+
  scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
  guides(color = guide_legend(override.aes = list(size=8))) #legend size
pool8.barcodes.snagmap
#ggsave(filename = "pool8.barcodes.snagmap.png", plot = pool8.barcodes.snagmap, dpi = 1000)

# Dealing with location codes
ldat <- read.csv("locationdata.csv")
dim(ldat)
ldat <- ldat[ldat$Location.code %in% pool8.barcodes$lcode,] 
ldat
  # this has information on some of the locations in our data, but most of ours are randomly sampled locations and are not included here.

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

# Create subsetted data frame with location and barcode information for easier comparison
loc <- pool8.barcodes[,names(pool8.barcodes) %in% c("barcode", "site", "fstation", "sitetype", "pool", "utmzone", "utm_e", "utm_n", "gisgrid", "lon", "lat", "lcode")]

# How many fixed sites do we have?
a <- pool8.barcodes[pool8.barcodes$sitetype == "subj.perm" & !is.na(pool8.barcodes$sitetype), ]
a <- droplevels(a)
table(a$lcode) #there are literally only two location codes that are fixed.
table(a$barcode) #but there are a bunch of barcodes, which makes sense if each of the locations was visited multiple times. 

#  plot fixed sites only 
pool8.fixedsites <- ggmap(m)+
  ggtitle("Pool 8 Fixed Sites")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[pool8.barcodes$sitetype == "subj.perm" & !is.na(pool8.barcodes$sitetype), ], aes(x = lon, y = lat), size = 1, alpha = 0.8, color = "red")
pool8.fixedsites

# check across all the data, not just pool8
fishdat <- read.csv("ltrm_fish_data.csv")
b <- droplevels(fishdat[fishdat$sitetype == "2" & !is.na(fishdat$sitetype),])
table(b$lcode)
# sure enough, there are more permanent sites in the other pools, just not pool 8.

# make another plot color-coded by site type
#get map of Pool 8
m <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), zoom = 11, maptype = "terrain", source = "google")
ggmap(m)

#plot map with barcodes by site type
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

# plot map of CWD by time
ggmap(m, extent = "panel", legend = "bottomright") +
  # plot the lat and lon points of all CWD
  # use sdate because sdate and fdate were the same
  geom_point(aes(x = lon, y = lat, color = as.numeric(sdat)),  data = pool8.barcodes %>% filter(snag == 1), size = .5, alpha = .8) +
  scale_x_continuous(limits = c(-91.4, -91.1)) +
  # set the color to get more intense as the date becomes more recent
  scale_colour_gradient(low = "#FFEBEE", high = "#C62828", breaks = c(7500,10000,12500,15000), labels = c("07/15/90", "05/19/97", "03/23/04", "01/26/11")) +
  ggtitle("Map of CWD Presence by Time") + xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(color="#C62828", size=14, face="bold")) +
  labs(color = "Date of Sampling")
#ggsave("CWDTime.png", plot = last_plot(), dpi = 1000)
