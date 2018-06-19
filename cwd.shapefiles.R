# Working with aquatic habitat shape files 
source("libraries.R")
# show the files available to read
list.files("aquahab/", pattern='\\.shp$')
# does the file exist?
file.exists('aquahab/p8_1989_aquahab.shp')

# read the file in. Need to have all the files (.sbn, .sbx, .dbf, .prj) in the same folder, for some reason. Also, remember to *leave off the .shp extension on the shapefile!!!!!!*
aquahab <- readOGR(dsn = "aquahab", layer = "p8_1989_aquahab")
glimpse(aquahab)

# transform coordinates to spatial points
points <- SpatialPoints(pool8.barcodes[,c("utm_e", "utm_n")])
proj4string(aquahab)
proj4string(points) <- "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#extract data for points (this gets a data frame)
ext <- over(x = points, y = aquahab)

# add columns to pool8.barcodes and save the file
load("pool8.barcodes.Rda")
pool8.barcodes$aqua_code <- ext$AQUA_CODE
pool8.barcodes$aqua_desc <- ext$AQUA_DESC
pool8.barcodes$aqua_shortname <- recode_factor(pool8.barcodes$aqua_code, 
                                               `CACL` = "CFL--aband. channel lk",
                                               `CFDL` = "CFL--floodplain dep. lk", 
                                               `CFSA` = "CF--shallow aq. area", 
                                               `CIMP` = "Ctg. imp. area", 
                                               `IACL` = "IFL--aband. channel lk", 
                                               `IBP` = "IFL--borrow pit", 
                                               `IFDL` = "IFL--floodplain dep. lk", 
                                               `MCB` = "MC--channel border", 
                                               `MNC` = "MC--nav. channel", 
                                               `NOPH` = "NOPH", 
                                               `SC` = "2º channel", 
                                               `TRC` = "Trib. channel", 
                                               `N` = "Non-aq. area")
pool8.barcodes <- droplevels(pool8.barcodes)
#save(pool8.barcodes, file = "pool8.barcodes.Rda")

# Plotting aquatic habitats with colors, following this tutorial: http://mazamascience.com/WorkingWithData/?p=1494
library(rgdal)
library(rgeos)
library(ggplot2)
library(scales)
library(maptools)
# add to data a new column termed "id" composed of the rownames of data
aquahab@data$id <- aquahab@data$OBJECTID

# create a data.frame from our spatial object
polygons.df <- fortify(aquahab, region = "OBJECTID")

# merge the "fortified" data with the data from our spatial object
aquahabdf <- merge(polygons.df, aquahab@data, by = "id")

gghabs <- ggplot(data = aquahabdf, aes(x=long, y=lat, fill = AQUA_CODE, group = group)) +
  geom_polygon()  +
  coord_equal()
print(gghabs)
#note that you have to use the group = group parameter to get the polygons to plot in the right order. Don't quite know what it means, but it's essential. 
