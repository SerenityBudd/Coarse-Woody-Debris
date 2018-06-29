aquahab_names <- c("Contiguous Floodplain Lake-Abandoned Channel Lake", 
                   "Contiguous Floodplain Lake-Floodplain Depression Lake", 
                   "Contiguous Floodplain Shallow Aquatic Area", 
                   "Contiguous Impounded Area", 
                   "Isolated Floodplain Lake-Abandoned Channel Lake", 
                   "Isolated Floodplain Lake-Borrow Pit", 
                   "Isolated Floodplain Lake-Floodplain Depression Lake", 
                   "Isolated Floodplain Lake-Manmade Lake", 
                   "Isolated Floodplain Lake-Tributary Delta Lake", 
                   "Main Channel-Channel Border", 
                   "Main Channel-Navigation Channel", 
                   "Non-Aquatic Area", 
                   "Secondary Channel", 
                   "Tertiary Channel", 
                   "Tributary Channel", 
                   "No Photo Coverage")

aquahab_codes <- c("CACL", "CFDL", "CFSA", "CIMP", "IACL", "IBP",  "IFDL", "IMML", "ITDL", "MCB", "MNC", "N", "SC", "TC", "TRC", "NOPH")

strata_names <- c("Backwater, Contiguous Shoreline", 
                  "Impounded--Offshore", 
                  "Impounded--Shoreline", 
                  "Main Channel Border--Unstructured", 
                  "Main Channel Border--Wing Dam Area", 
                  "Side Channel Border", 
                  "Tailwater Zone") 

# Colors for aquatic habitat types based on the aquahab polygons
myColors <- c("#09BF2B", "#0CC891", "#08A4BC", "#1071C1", "#AD0B98", "#AD0B47", "#D685A3", "#AD200B", "#C24875", "#200BAD", "#0B47AD", "#9B783C", "#678CCC", "#B3C6E6", "#C596EB", "#808080")
names(myColors) <- aquahab_names

myColors_codes <- c("#09BF2B", "#0CC891", "#08A4BC", "#1071C1", "#AD0B98", "#AD0B47", "#D685A3", "#AD200B", "#C24875", "#200BAD", "#0B47AD", "#9B783C", "#678CCC", "#B3C6E6", "#C596EB", "#808080")
names(myColors_codes) <- aquahab_codes

# Colors for aquatic habitat strata, based on previous colors
strataColors <- c("#08A4BC", "#0CC891", "#1071C1", "#0B47AD", "#200BAD", "#678CCC", "#AD0B98")
names(strataColors) <- strata_names

# More distinct colors for aquatic habitat strata
strataColors_distinct <- c("chartreuse3", "blue", "dodgerblue2", "red", "darkred", "orange", "magenta")
names(strataColors_distinct) <- strata_names

# More distinct colors for aquatic habitat strata
lcColors_lumped <- c("cyan2", "darkgray", "forestgreen", "lawngreen", "navajowhite2")
names(lcColors_lumped) <- c("Aquatic veg", "Developed", "Forest", "Grassland or meadow", "Sand")
