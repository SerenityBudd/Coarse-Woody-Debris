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
inters <- intersect(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in LTRM data not in fish traits
ltrmf <- setdiff(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in the fishtraits not the LTRM data
traitsf <- setdiff(fishinfo[,"Fishcode"], FishCodes[,1])

# all the U- names are "unidentified fish type", hopefully keep some of these
#        GET RID
# UNID is generally unidentified
# I am assuming WSSN is supposed to be WDSN
# YOYF is age-0 fish
# LRVL is larval
# NFSH is no fish caught
# SCBC could be SCBS NOT confident

# remove the rows of fishcodes not in the ltrm data
fishinfo <- filter(fishinfo, Fishcode %in% inters)
identical(as.character(fishinfo[,1]), inters)

# the fish info data says the LTRM proj has not collected these
nos <- fishinfo[fishinfo$LTRMP == "N","Fishcode"]
# they have, so we will keep them
summary(fishdat[fishdat$fishcode %in% nos,"fishcode"])

# remove the rows of fishcodes that are not in the fish info data
summary(fishdat[fishdat$fishcode %in% ltrmf,"fishcode"])
LTRMrm <- c("LRVL", "NFSH", "SCBC", "U-IL", "U-PC", "UNID", "WSSN", "YOYF")

ltrmfishdat <- filter(fishdat, !fishcode %in% LTRMrm)

# explore the fishinfo data

myfunc <- function (vec) {
  table(vec, exclude = F)
}

apply(fishinfo, 2, myfunc)


# plot things

load("pool8.barcodes.Rda")
#pool8.barcodes$snag <- as.numeric(pool8.barcodes$snag)
pool8.b <- pool8.barcodes[,c("fishcode", "snag")]
pool8.b <- pool8.b[order(pool8.b$fishcode),] 
pool8.b <- pool8.b[-c(1:204),]
pool8.bb <- as.data.frame(table(pool8.b))

pool8.bb <- data.frame(pool8.bb[2:58, c("fishcode", "Freq")], 
                       pool8.bb[60:116, "Freq"])
colnames(pool8.bb) <- c("Fishcode", "nosnag", "snag")

pool8.fishtraits <- inner_join(pool8.bb, fishinfo, by = "Fishcode")


ggplot(data = filter(pool8.fishtraits,!is.na(snag) & !is.na(nosnag)), aes(x = Fishcode, y = snag/(snag+nosnag))) +
  geom_point() +
  theme(axis.text.x = element_text(size = 5, angle=60))


        