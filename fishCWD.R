source("libraries.R")

# import data on fish, sort the dataframes by fishcode
fishdist <- read.csv("Table_Distribution.csv")[,-c(2:5)]
fishdist <- fishdist[order(fishdist$Fishcode),] 

fishgrowth <- read.csv("Table_Growth.csv")
fishgrowth <- fishgrowth[order(fishgrowth$Fishcode),] 

species <- read.csv("Table_LTRMP_Species_List.csv")
species <- species[order(species$Fishcode),] 

fishmisc <- read.csv("Table_Miscellaneous.csv")
fishmisc <- fishmisc[order(fishmisc$Fishcode),] 

fishreproduction <- read.csv("Table_Reproduction.csv")
fishreproduction <- fishreproduction[order(fishreproduction$Fishcode),] 

fishtraits <- read.csv("Table_Preference_and_Guild.csv")
fishtraits <- fishtraits[order(fishtraits$Fishcode),] 

# explore the date using dim and str
dim(fishdist)
dim(fishgrowth)
dim(species)
dim(fishmisc)
dim(fishreproduction)
dim(fishtraits)

str(fishdist) # ehh
str(fishgrowth) # ehhhhhhhh
str(species) # good
str(fishmisc) # good
str(fishreproduction) # ehhh
str(fishtraits) # good

# notice that the first col of every dataframe is the same
identical(fishdist$Fishcode, fishgrowth$Fishcode, 
          species$Fishcode, fishmisc$Fishcode, 
          fishreproduction$Fishcode, fishtraits$Fishcode)

# combine the important dataframes into fishinfo
fishinfo <- left_join(left_join(species, fishmisc, by = "Fishcode"), fishtraits, by = "Fishcode")

dim(fishinfo)
str(fishinfo)


table(fishdist$Mid.Range.Latitude, exclude = F)
table(fishdist$Mean.Range.Latitude, exclude = F)
table(fishdist$Ubiquity, exclude = F)

# the same fish not in either latitude col
identical(fishdist[is.na(fishdist$Mid.Range.Latitude),], 
          fishdist[is.na(fishdist$Mean.Range.Latitude),])

# these are the NA fish
fishdist[is.na(fishdist$Mid.Range.Latitude),1]

fishinfo$LTRMP.Ten.Year.Rank


fishdat2 <- sort(fishdat$fishcode)
