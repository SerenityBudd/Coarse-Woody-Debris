## MAPPING
# POOL 8 SNAG PRESENCE/ABSENCE BY BARCODE
source("libraries.R")

#read in the pool8 barcodes data.
load("pool8.barcodes.Rda")
str(pool8.barcodes)

# `snag` is an integer; let's change it to a factor
pool8.barcodes$snag <- factor(pool8.barcodes$snag)

#get map of Pool 8
m <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), zoom = 11, maptype = "roadmap", source = "google")
ggmap(m)

# plot map with points on it 
pool8.barcodes.snagmap <- ggmap(m)+
  ggtitle("CWD in Pool 8")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
             aes(x = lon, y = lat, color = snag, pch = snag), 
             size = .7, 
             alpha = 0.8)+
  scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
  guides(color = guide_legend(title = "Coarse\nWoody\nDebris",
                              override.aes = list(size=8)),
         pch = guide_legend(title = "Coarse\nWoody\nDebris",
                              override.aes = list(size=8)))+ #legend size
  theme(text = element_text(size=20))
pool8.barcodes.snagmap
#ggsave(filename = "pool8.barcodes.snagmap.png", plot = pool8.barcodes.snagmap, dpi = 1000)

# data frame for box
boxdf <- data.frame(x1=c(-91.292), x2=c(-91.188), y1=c(43.68), y2=c(43.757))

# plot map with box to zoom into 
pool8.barcodes.snagmap.box <- ggmap(m)+
  ggtitle("CWD in Pool 8")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
             aes(x = lon, y = lat, color = snag, pch = snag), 
             size = .7, 
             alpha = 0.8)+
  scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
  guides(color = guide_legend(title = "Coarse\nWoody\nDebris",
                              override.aes = list(size=8)),
         pch = guide_legend(title = "Coarse\nWoody\nDebris",
                            override.aes = list(size=8))) + 
  geom_rect(inherit.aes = FALSE, data=boxdf, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha = 0, color="black")+
  theme(text = element_text(size=20))
pool8.barcodes.snagmap.box
#ggsave(filename = "pool8.barcodes.snagmap.box.png", plot = pool8.barcodes.snagmap.box, dpi = 1000)

# get a zoomed in map of pool8
mm <- get_map(location = c(-91.24, mean(range(pool8.barcodes$lat))), zoom = 13, maptype = "roadmap", source = "google")
ggmap(mm)

# plot map of pool8 zoomed in
pool8.barcodes.snagmap.zoom <- ggmap(mm)+
  ggtitle("CWD in Pool 8")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
             aes(x = lon, y = lat, color = snag, pch = snag), 
             size = 1, 
             alpha = 0.8)+
  scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
  guides(color = guide_legend(title = "Coarse\nWoody\nDebris",
                              override.aes = list(size=8)),
         pch = guide_legend(title = "Coarse\nWoody\nDebris",
                            override.aes = list(size=8))) + #legend size
  geom_rect(inherit.aes = FALSE, data=boxdf, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), alpha = 0, color="black")+
  theme(text = element_text(size=20))
pool8.barcodes.snagmap.zoom
#ggsave(filename = "pool8.barcodes.snagmap.zoom.png", plot = pool8.barcodes.snagmap.zoom, dpi = 1000)

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

# get a black and white map of pool8
mmm <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), zoom = 11, maptype = "roadmap", source = "google", color="bw")
ggmap(mmm)

# make another plot color-coded by site type
pool8.barcodes.sitetype <- ggmap(mmm)+
  ggtitle("Pool 8 Sampling Sites by Site Type")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$sitetype),], 
             aes(x = lon, y = lat, color = sitetype), size = 0.65, alpha = 0.8, pch = 20)+
  scale_color_manual(name = "Site Type", labels = c("Primary Random", "Alternate Random",
                                                    "Subjective Permanent"), 
                     values=c("#D41A1A", "#1A1AD4", "#FFC300"))+
  guides(color = guide_legend(override.aes = list(size=10))) +
  # add the sub perm sites on top in a bigger size
  geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$sitetype) &
                                     pool8.barcodes$sitetype == "subj.perm",], 
             size = 1.5, alpha = 0.8, pch = 20, color = "#FFC300")+
  theme(text = element_text(size=20))
pool8.barcodes.sitetype
#ggsave(filename = "pool8.barcodes.sitetype.png", plot = pool8.barcodes.sitetype, dpi = 1000)

# Map color coded by wingdyke presence
#plot map with barcodes by site type
wingdyke <- ggmap(m)+
  ggtitle("Wing Dams/Dykes in Pool 8")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$wingdyke),], 
             aes(x = lon, y = lat, color = factor(wingdyke)), 
             size = 1, 
             alpha = 0.8, 
             pch = 20)+
  scale_color_manual(name = "", labels = c("No Dam at Site", "Dam/Dyke at Site"), values=c("#FF8300", "#5D00C6"))+
  guides(color = guide_legend(override.aes = list(size=10)))+
  theme(text = element_text(size=20))
wingdyke
#ggsave(filename = "wingdyke.png", plot = wingdyke, dpi = 1000)

# plot map of CWD by time
pool8.cwd.timemap <- ggmap(mmm, extent = "panel", legend = "bottomright") +
  # plot the lat and lon points of all CWD
  # use sdate because sdate and fdate were the same
  geom_point(aes(x = lon, y = lat, color = as.numeric(sdat)),  
             data = pool8.barcodes %>% filter(snag == 1), 
             size = .5, alpha = .8, pch = 19) +
  scale_x_continuous(limits = c(-91.4, -91.1)) +
  # set the color to get brighter as the date becomes more recent
  scale_colour_gradient(low = "#400036", high = "#FF00D8", 
                        breaks = c(7500,10000,12500,15000), 
                        labels = c("07/15/90", "05/19/97", "03/23/04", "01/26/11")) +
  ggtitle("Map of CWD Presence by Time") + 
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(plot.title = element_text(color="#600051", size=14, face="bold")) +
  labs(color = "Date of Sampling")+
  theme(text = element_text(size=20))
pool8.cwd.timemap
#ggsave(filename = "pool8.cwd.timemap.png", plot = pool8.cwd.timemap, dpi = 1000)


LOESS <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = lat, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.2, pch = 20)+
  stat_smooth(method = "loess", color = "darkred", size = 1.5) +
  xlab("N \n   \n  (Latitude)  \n  \n S") +
  ylab("Snag Presence") +
  theme(axis.text.x = element_text(angle=270), 
        axis.title.x =  element_text(angle=270, vjust = .5),
        axis.text.y = element_text(angle=270), 
        axis.title.y =  element_text(angle=270))+
  theme(text = element_text(size=20))
LOESS
#ggsave(filename = "LOESS.png", plot = LOESS, dpi = 1000)

secchim <- ggmap(m)+
  ggtitle("Pool 8 Sampling Sites by Secchi Depth")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(limits = c(-91.4, -91.1))+
  geom_point(data = pool8.barcodes[abs(!is.na(pool8.barcodes$secchi)) & 
                                     pool8.barcodes$secchi >= 0,], 
             aes(x = lon, y = lat, color = secchi), 
             size = 1, 
             alpha = 0.8, 
             pch = 20)+
  theme(text = element_text(size=20))#+
#  scale_color_manual(name = "Secchi Level", labels = c("No Wing Dam/Dyke Present", "Wing Dam/Dyke Present"), values=c("#FF8300", "#5D00C6"))+
 # guides(color = guide_legend(override.aes = list(size=10)))
secchim


load("newdat.Rda")
#Create a custom color scale with colors mapped to aquatic habitat types
strataColors <- c("#08A4BC", "#0CC891", "#1071C1", "#0B47AD", "#200BAD", "#678CCC", "#AD0B98")
names(strataColors) <- levels(newdat$stratum_name)

ggplot(data = newdat, aes(x = lon, y = lat, color = stratum_name))+
  geom_point(size = 0.6)+
  scale_color_manual(values = strataColors)
#these colors correspond more closely to the original map but they aren't distinctive enough for this graph

#another attempt:
strataColors2 <- c("chartreuse3", "blue", "dodgerblue2", "red", "darkred", "orange", "magenta")
names(strataColors) <- levels(newdat$stratum_name)

ggplot(data = newdat, aes(x = lon, y = lat, color = stratum_name))+
  geom_point(size = 0.6)+
  scale_color_manual(values = strataColors2)
