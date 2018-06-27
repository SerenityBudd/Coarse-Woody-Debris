# Working with aquatic habitat shape files 
source("libraries.R")
load("data/pool8.barcodes.Rda")
# show the files available to read
list.files("aquahab/", pattern='\\.shp$')
# does the file exist?
file.exists('aquahab/p8_1989_aquahab.shp')

# read the file in. Need to have all the files (.sbn, .sbx, .dbf, .prj) in the same folder, for some reason. Also, remember to *leave off the .shp extension on the shapefile!!!!!!*
aquahab <- readOGR(dsn = "aquahab", layer = "p8_1989_aquahab")
glimpse(aquahab)
aquahab@proj4string #this is in UTM, zone 15. 
aquahab <- spTransform(aquahab, "+proj=utm +zone=15 +datum=NAD27 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

aquahab@data$AQUA_CODE <- factor(aquahab@data$AQUA_CODE, levels(aquahab@data$AQUA_CODE)[c(1:12, 14:16, 13)])
aquahab@data$AQUA_DESC <- factor(aquahab@data$AQUA_DESC, levels(aquahab@data$AQUA_DESC)[c(1:11, 13:16, 12)])

# transform coordinates to spatial points
points <- SpatialPoints(pool8.barcodes[,c("utm_e", "utm_n")])
plot(aquahab)
plot(points, add = T)
proj4string(aquahab) #equivalent call to `aquahab@proj4string`. UTM zone 15
proj4string(points) #no projection for the points yet
proj4string(points) <- proj4string(aquahab)
proj4string(points) #now has same projection
#may have to have the same projection in order to plot together. 

#extract data for points (this gets a data frame)
ext <- over(x = points, y = aquahab)

# add columns to pool8.barcodes and save the file
pool8.barcodes$aqua_code <- ext$AQUA_CODE
pool8.barcodes$aqua_desc <- ext$AQUA_DESC
#save(pool8.barcodes, file = "pool8.barcodes.Rda")
load("data/pool8.barcodes.Rda")
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
source("color_schemes.R")

#transform sampling points
pointslonlat <- as.data.frame(spTransform(points, CRS("+proj=longlat +datum=WGS84")))

#names(pointslonlat) <- c("lon", "lat")

gghabs <- ggplot(data = aquahabdf) +
  geom_polygon(aes(x = lon, y = lat, fill = AQUA_CODE, group = group)) +
  coord_equal(ratio = 1.2)+
  scale_fill_manual(name = "Aquatic Habitat Type", values = myColors)+
  ggtitle("Aquatic Habitat Types in Pool 8")
print(gghabs)
#ggsave("aquahabs.2000.png", plot = gghabs, dpi = 2000)
#ggsave("aquahabs.500.png", plot = gghabs, dpi = 500)

gghabs_wpoints <- ggplot(data = aquahabdf) +
  geom_polygon(aes(x = lon, y = lat, fill = AQUA_CODE, group = group)) +
  coord_equal(ratio = 1.2)+
  scale_fill_manual(name = "Aquatic Habitat Type", values = myColors)+
  ggtitle("Aquatic Habitat Types in Pool 8")+
  geom_point(data = pointslonlat, aes(x = lon, y = lat), size = 0.2, alpha = 0.5, pch = 20)
print(gghabs_wpoints)

#note that there's misalignment between the points and the habitat areas!
#ggsave("aquahabs_wpoints.2000.png", plot = gghabs_wpoints, dpi = 2000)
#ggsave("aquahabs_wpooints.500.png", plot = gghabs_wpoints, dpi = 500)

#note that you have to use the group = group parameter to get the polygons to plot in the right order. Don't quite know what it means, but it's essential. 

# Characterizing the habitat types
#we're accounting for a lot of the variability in depth by adjusting for habitat type
#show density curves for each habitat type and depth: like the histograms, but all together. Do this in general for a lot of different variables: characterize the habitat types. 
#do lots of univariate/bivariate models to predict CWD

#how many points do we have per aquatic habitat type?
table(pool8.barcodes$aqua_code)
#take out the ones with fewer than ten
p8b <- pool8.barcodes[!pool8.barcodes$aqua_code %in% c("IACL", "IBP", "IFDL", "IMML", "ITDL", "TC", "NOPH", NA),]
p8b$secchi <- abs(p8b$secchi)

#which ones was that?
notenough <- c("IACL", "IBP", "IFDL", "IMML", "ITDL", "TC", "NOPH", NA)
#how many points do we have per aquatic habitat type now?
table(p8b$aqua_code, exclude = notenough) #`exclude =` allows you to specify which factor levels to leave out

#boxplot of depth
shortnames <- c("Abandoned Channel Lk", "Floodplain Depression Lk", "Shallow Aquatic Area", "Impounded Area", "Main Channel Boundary", "Main Navigation Channel", "Non-aquatic Area", "Secondary Channel", "Tributary Channel")
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = depth, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Water Depth (meters)")+
  ggtitle("Water Depth by Aquatic Habitat Type")

#density plot of depth
ggplot(data = p8b, aes(x = depth))+
  geom_line(stat = "density", aes(color = aqua_code), 
               size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Water Depth (meters)")+
  ylab("Density")+
  ggtitle("Water Depth Distributions by Aquatic Habitat Type")

#density plot of current
ggplot(data = p8b, aes(x = current))+
  geom_line(stat = "density", aes(color = aqua_code), 
               size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Current (m/s)")+
  ylab("Density")+
  ggtitle("Current Distributions by Aquatic Habitat Type")

#boxplot of current
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = current, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Current (m/s)")+
  ggtitle("Current by Aquatic Habitat Type")

#boxplot of secchi depth
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = secchi, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Secchi Depth (cm)")+
  ggtitle("Secchi Depth by Aquatic Habitat Type")

#density plot of secchi depth
ggplot(data = p8b, aes(x = secchi))+
  geom_line(stat = "density", aes(color = aqua_code), 
            size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Secchi Depth (cm)")+
  ylab("Density")+
  ggtitle("Secchi Depth Distributions by Aquatic Habitat Type")

#boxplot of dissolved oxygen (do)
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = do, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Dissolved Oxygen (mg/L)")+
  ggtitle("Dissolved Oxygen by Aquatic Habitat Type")

#density plot of dissolved oxygen (do)
ggplot(data = p8b, aes(x = do))+
  geom_line(stat = "density", aes(color = aqua_code), 
            size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Dissolved Oxygen (mg/L)")+
  ylab("Density")+
  ggtitle("Dissolved Oxygen Distributions by Aquatic Habitat Type")

#boxplot of temperature
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = temp, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Water Temperature (ºC)")+
  ggtitle("Water Temperature by Aquatic Habitat Type")

#density plot of temperature
ggplot(data = p8b, aes(x = temp))+
  geom_line(stat = "density", aes(color = aqua_code), 
            size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Water Temperature (ºC)")+
  ylab("Density")+
  ggtitle("Temperature Distributions by Aquatic Habitat Type")

#boxplot of conductivity
ggplot(data = p8b, 
       aes(x = droplevels(aqua_code), 
           y = cond, 
           fill = aqua_code))+
  geom_boxplot()+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    values = myColors, 
                    labels = shortnames)+
  xlab("Aquatic Habitat Type")+
  ylab("Conductivity (Siemens/cm)")+
  ggtitle("Conductivity by Aquatic Habitat Type")

#density plot of conductivity
ggplot(data = p8b, aes(x = cond))+
  geom_line(stat = "density", aes(color = aqua_code), 
            size = 1)+
  scale_color_manual(name = "Aquatic Habitat Type", 
                     values = myColors, 
                     labels = shortnames)+
  xlab("Conductivity (Siemens/cm)")+
  ylab("Density")+
  ggtitle("Conductivity Distributions by Aquatic Habitat Type")


### More recent shapefile
hab2010 <- readOGR(dsn = "2010_armycorps_shapefile/", layer = "aqa_2010_lvl3_011918")
glimpse(hab2010)
#could try subsetting this and setting the bounding box according to min/max utm values from pool8
