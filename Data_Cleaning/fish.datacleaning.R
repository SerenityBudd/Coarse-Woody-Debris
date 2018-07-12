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
fishinfo <- left_join(Species, fishmisc, by = "Fishcode") %>%
  left_join(., fishtraits, by='Fishcode') %>%
  left_join(., fishreproduction, by = "Fishcode") %>%
  left_join(., fishgrowth, by = "Fishcode") %>%
  left_join(., fishdist, by = "Fishcode")

dim(fishinfo)
str(fishinfo)

# grabbing the unique fishcodes from the LTRM data
FishCodes <- as.data.frame(unique(sort(fishdat$fishcode))[-1])
colnames(FishCodes) <- c("Fishcode")

# the fishcodes in both the LTRM data and fish traits
inters <- intersect(FishCodes[,1], fishinfo[,"Fishcode"])

# remove the rows of fishcodes not in the ltrm data
fishinfo <- filter(fishinfo, Fishcode %in% inters)
identical(as.character(fishinfo[,1]), inters)

# the fish info data says the LTRM proj has not collected these
nos <- fishinfo[fishinfo$LTRMP == "N","Fishcode"]
# they have, so we will keep them
fishdat[fishdat$fishcode %in% nos,"fishcode"]

# remove the rows of fishcodes that are not in the fish info data
ltrmfishdat <- filter(fishdat, fishcode %in% inters) 
# make snag a factor
ltrmfishdat$snag01 <- ltrmfishdat$snag
ltrmfishdat$snag <- factor(ltrmfishdat$snag, levels = c(0,1),
                           labels = c("No", "Yes"))
# make a column for the stratum name
ltrmfishdat$stratum_name <- ltrmfishdat$stratum
levels(ltrmfishdat$stratum_name) <- c("Backwater, Contiguous Offshore","Backwater, Contiguous Shoreline","Main Channel Border, Wingdam, Partial", "Main Channel Trough", "Impounded, Offshore", "Impounded, Shoreline", "Main Channel Border, Unstructured", "Main Channel Border, Wing Dam Area", "Side Channel Border","Tributary", "Tailwater Zone","Unexploded Ordinance Area, Pool 13")
## "Main Channel Border, Wingdam, Partial"
  ## partial means partial data, all snag is NA, exclude
## "Unexploded Ordinance Area - Pool 13"
  ## not a habitat, located only in pool 13, exclude
## "Main Channel Trough"
  ## all snag is NA, exclude

ltrmfishdat <- ltrmfishdat %>% 
  # remove the stratum for the above reasons
  filter(!stratum_name %in% c("Main Channel Border, Wingdam, Partial", "Unexploded Ordinance Area, Pool 13", "Main Channel Trough")) %>%
  # only in sites where they electroshocked for fish during the day
  filter(gear %in% "D") %>%
  # remove the rows where snag is NA
  filter(!is.na(snag)) %>%
  # remove rows not in a pool
  filter(!pool %in% "") %>%
  # drop the unused levels
  droplevels()
#save(ltrmfishdat, file = "data/ltrmfishdat.Rda")

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

#save(fishinfo, file = "data/fishinfo.Rda")
