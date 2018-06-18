# Working with aquatic habitat shape files 
library(rgdal)

# show the files available to read
list.files(".", pattern='\\.shp$')
# does the file exist?
file.exists('p8_1989_aquahab.shp')

# read the file in. Need to have all the files (.sbn, .sbx, .dbf, .prj) in the same folder, for some reason. Also, remember to *leave off the .shp extension on the shapefile!!!!!!*
aquahab <- readOGR(dsn = ".", layer = "p8_1989_aquahab")
glimpse(aquahab)

# transform coordinates to spatial points
points <- SpatialPoints(pool8.barcodes[,c("utm_e", "utm_n")])
proj4string(aquahab)
proj4string(points) <- "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#extract data for points (this gets a data frame)
ext <- over(x = points, y = aquahab)
