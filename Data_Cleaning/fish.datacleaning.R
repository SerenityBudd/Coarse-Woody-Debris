source("libraries.R")

fishdat <- read.csv("data/ltrm_fish_data.csv")

# import data on fish, sort the dataframes by fishcode
fishdist <- read.csv("data/Table_Distribution.csv")[,-c(2:5)]
fishdist <- fishdist[order(fishdist$Fishcode),] 

fishgrowth <- read.csv("data/Table_Growth.csv")
fishgrowth <- fishgrowth[order(fishgrowth$Fishcode),]

Species <- read.csv("data/Table_LTRMP_Species_List.csv")
Species <- Species[order(Species$Fishcode),] 

fishmisc <- read.csv("data/Table_Miscellaneous.csv")
fishmisc <- fishmisc[order(fishmisc$Fishcode),] 

fishreproduction <- read.csv("data/Table_Reproduction.csv")
fishreproduction <- fishreproduction[order(fishreproduction$Fishcode),] 

fishtraits <- read.csv("data/Table_Preference_and_Guild.csv")
fishtraits <- fishtraits[order(fishtraits$Fishcode),] 

# explore the date using dim and str
dim(fishdist)
dim(fishgrowth)
dim(Species)
dim(fishmisc)
dim(fishreproduction)
dim(fishtraits)

str(Species) 
str(fishmisc) 
str(fishtraits) 
str(fishreproduction) 
str(fishgrowth) 
str(fishdist)

# combine the important dataframes into fishinfo
fishinfo <- left_join(left_join(Species, fishmisc, by = "Fishcode"), fishtraits, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishreproduction, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishgrowth, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishdist, by = "Fishcode")

dim(fishinfo)
str(fishinfo)

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
ltrmfishdat <- filter(fishdat, fishcode %in% inters)

#Update fish names to current taxonomy (based on Google searches) and correct spelling errors

fishinfo$Scientific.Name <- as.character(fishinfo$Scientific.Name)

fishinfo$Scientific.Name.Current <- fishinfo$Scientific.Name

fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Lampetra appendix"] <- "Lethenteron appendix"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Lepisosteus spatula"] <- "Atractosteus spatula"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Notropis amblops"] <- "Hybopsis amblops"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                        "Hypopthalmichthys nobilis"] <- "Hypophthalmichthys nobilis"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Notropis hubbsi"] <- "Pteronotropis hubbsi"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Ammocrypta asprella"] <- "Crystallaria asprella"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Etheostoma proelaire"] <- "Etheostoma proeliare"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Myoxocephalus thompsoni"] <- "Myoxocephalus thompsonii"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Cottus bairdi"] <- "Cottus bairdii"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Phoxinus eos"] <- "Chrosomus eos"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Notropis amnis"] <- "Hybopsis amnis"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Notropis fumeus"] <- "Lythrurus fumeus"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Chologaster agassizi"] <- "Forbesichthys agassizii"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Percina uranidie"] <- "Percina uranidea"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Sander canadense"] <- "Sander canadensis"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Phoxinus erythrogaster"] <- "Chrosomus erythrogaster"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                      "Hypopthalmichthys molitrix"] <- "Hypophthalmichthys molitrix"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Sander vitreum"] <- "Sander vitreus"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Catostomus commersoni"] <- "Catostomus commersonii"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Moxostoma duquesnei"] <- "Moxostoma duquesnii"
fishinfo$Scientific.Name.Current[fishinfo$Scientific.Name.Current == 
                           "Notropis buccatus"] <- "Ericymba buccata"

save(fishinfo, file = "data/fishinfo.Rda")
