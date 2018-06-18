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

str(fishdist)
str(fishgrowth) 
str(species) 
str(fishmisc) 
str(fishreproduction) 
str(fishtraits) 

# notice that the first col of every dataframe is the same
identical(fishdist$Fishcode, fishgrowth$Fishcode, 
          species$Fishcode, fishmisc$Fishcode, 
          fishreproduction$Fishcode, fishtraits$Fishcode)

# combine the important dataframes into fishinfo
fishinfo <- left_join(left_join(species, fishmisc, by = "Fishcode"), fishtraits, by = "Fishcode")

dim(fishinfo)
str(fishinfo)


# exploring the fish distribution data
table(fishdist$Mid.Range.Latitude, exclude = F)
table(fishdist$Mean.Range.Latitude, exclude = F)
table(fishdist$Ubiquity, exclude = F)

# the same fish are not in either latitude col
identical(fishdist[is.na(fishdist$Mid.Range.Latitude),], 
          fishdist[is.na(fishdist$Mean.Range.Latitude),])

# these are the fish that are NA for both Lat cols
fishdist[is.na(fishdist$Mid.Range.Latitude),1]

# grabbing the unique fishcodes from the LTRM data
FishCodes <- as.data.frame(unique(sort(fishdat$fishcode))[-1])
colnames(FishCodes) <- c("Fishcode")

# the fishcodes in both the LTRM data and fish traits
intersect(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in LTRM data not in fish traits
ltrmf <- setdiff(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in the fishtraits not the LTRM data
setdiff(fishinfo[,"Fishcode"], FishCodes[,1])

# knowing this we should think about removing data on fish that are not in both data sets, there is no point in knowing the traits of a fish if their arent int the UMR and we cannot do our analysis on UMR fish if we have no traits data on them

# checking to see if the ones only in LTRM are typos

# all the U- names are "unidentified fish type"
# UNID is generally unidentified
# I am assuming WSSN is supposed to be WDSN
# YOYF is age-0 fish
# LRVL is larval
# NFSH is no fish caught
# SCBC could be SCBS NOT confident

table(fishdat[fishdat$fishcode %in% ltrmf,"fishcode"])
