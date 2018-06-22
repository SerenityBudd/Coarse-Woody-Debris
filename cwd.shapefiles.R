# Working with aquatic habitat shape files 
source("libraries.R")
load("pool8.barcodes.Rda")
# show the files available to read
list.files("aquahab/", pattern='\\.shp$')
# does the file exist?
file.exists('aquahab/p8_1989_aquahab.shp')

# read the file in. Need to have all the files (.sbn, .sbx, .dbf, .prj) in the same folder, for some reason. Also, remember to *leave off the .shp extension on the shapefile!!!!!!*
aquahab <- readOGR(dsn = "aquahab", layer = "p8_1989_aquahab")
glimpse(aquahab)
aquahab@data$AQUA_CODE <- factor(aquahab@data$AQUA_CODE, levels(aquahab@data$AQUA_CODE)[c(1:12, 14:16, 13)])
aquahab@data$AQUA_DESC <- factor(aquahab@data$AQUA_DESC, levels(aquahab@data$AQUA_DESC)[c(1:11, 13:16, 12)])

# transform coordinates to spatial points
points <- SpatialPoints(pool8.barcodes[,c("utm_e", "utm_n")])
plot(aquahab)
plot(points, add = T)
proj4string(aquahab)
proj4string(points) <- "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#extract data for points (this gets a data frame)
ext <- over(x = points, y = aquahab)

# add columns to pool8.barcodes and save the file
pool8.barcodes$aqua_code <- ext$AQUA_CODE
pool8.barcodes$aqua_desc <- ext$AQUA_DESC
#save(pool8.barcodes, file = "pool8.barcodes.Rda")
load("pool8.barcodes.Rda")
# Plotting aquatic habitats with colors, following this tutorial: http://mazamascience.com/WorkingWithData/?p=1494
# add to data a new column termed "id" composed of the rownames of data
aquahab@data$id <- aquahab@data$OBJECTID

# create a data.frame from our spatial object
polygons.df <- fortify(aquahab, region = "OBJECTID")

# merge the "fortified" data with the data from our spatial object
aquahabdf <- inner_join(polygons.df, aquahab@data, by = "id")
names(aquahabdf)[1:2] <- c("utm_e", "utm_n")

#project the utm easting and northing onto a CRS using utm zone 15
sputm <- SpatialPoints(aquahabdf[,c("utm_e", "utm_n")], proj4string = CRS("+proj=utm +zone=15 +datum=WGS84"))
#transform to latlon, save as a data frame
lonlat <- as.data.frame(spTransform(sputm, CRS("+proj=longlat +datum=WGS84")))
#rename the columns
names(lonlat) <- c("lon", "lat")
#join to the original data frame
aquahabdf <- cbind(aquahabdf, lonlat)

#Create a custom color scale with colors mapped to aquatic habitat types
myColors <- c("#09BF2B", "#0CC891", "#08A4BC", "#1071C1", "#AD0B98", "#AD0B47", "#D685A3", "#AD200B", "#C24875", "#200BAD", "#0B47AD", "#9B783C", "#678CCC", "#B3C6E6", "#C596EB", "#808080")
names(myColors) <- levels(aquahabdf$AQUA_DESC)

gghabs <- ggplot(data = aquahabdf, aes(x=lon, y=lat, fill = AQUA_DESC, group = group)) +
  geom_polygon() +
  coord_equal(ratio = 1.2)+
  scale_fill_manual(name = "Aquatic Habitat Type", values = myColors)+
  ggtitle("Aquatic Habitat Types in Pool 8")
print(gghabs)
ggsave("aquahabs.2000.png", plot = gghabs, dpi = 2000)
ggsave("aquahabs.500.png", plot = gghabs, dpi = 500)
#note that you have to use the group = group parameter to get the polygons to plot in the right order. Don't quite know what it means, but it's essential. 


### More recent shapefile
hab2010 <- readOGR(dsn = "2010_armycorps_shapefile/", layer = "aqa_2010_lvl3_011918")
glimpse(hab2010)
#could try subsetting this and setting the bounding box according to min/max utm values from pool8
