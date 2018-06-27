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

str(species) 
str(fishmisc) 
str(fishtraits) 
str(fishreproduction) 
str(fishgrowth) 
str(fishdist)

# notice that the first col of every dataframe is the same
identical(fishdist$Fishcode, fishgrowth$Fishcode, 
          species$Fishcode, fishmisc$Fishcode, 
          fishreproduction$Fishcode, fishtraits$Fishcode)

# combine the important dataframes into fishinfo
fishinfo <- left_join(left_join(species, fishmisc, by = "Fishcode"), fishtraits, by = "Fishcode")
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
summary(fishdat[fishdat$fishcode %in% ltrmf,"fishcode"])

ltrmfishdat <- filter(fishdat, !fishcode %in% ltrmf)

# explore the fishinfo data
for (i in 1:ncol(fishinfo)) {
  print(colnames(fishinfo)[i]) 
  print(summary(fishinfo[,i]))
}

#Quicky determine how many NA's are in each column of the data frame. 
nacount <- function(x){
  na.df <- data.frame(name = names(x), nas = NA)
  for(i in 1:ncol(x)){
    na.df$nas[i] <- sum(is.na(x[,i]))
  }
  return(na.df)
}

# count NA's
nacount(fishinfo)

# remove the columns with too many NAs
fishinfo <- select(fishinfo, -c(Animal, Wilcox.Ucrit, Freshwater.Marine, Maximum.LTRMP.Length, Substock:Trophy))

# selects only the columns that are numeric
fishcluster <- select(fishinfo, c(Fishcode, Exploit.Rank:Wilcox.Pass.Dams, Conservation.Status:Trophic.Guild, Water.Column.Preference:Egg.Bouyancy,Maximum.Fecundity:Mean.Incubation,Larval.Growth:Ubiquity))

# selects the columns that are important to cluster analysis
# method from paper
fishcluster1 <- fishinfo[,c("Common.Name", "Maximum.Literature.Length", "Length.at.Maturity", "Maximum.Age", "Age.at.Maturity", "Mean.Fecundity", "Mean.Ovum.Diameter", "Parental.Care")]

# removing repeat variables
fishcluster2 <- select(fishcluster, -c(Range.Ovum.Diameter,Adult.Trophic.Level, Maximum.Fecundity, Juvenile.Cutoff))

# remove variables with relatively more NAs
fishcluster3 <- select(fishcluster, -c(Range.Ovum.Diameter,Adult.Trophic.Level, Maximum.Fecundity, Juvenile.Cutoff,Egg.Bouyancy,Mean.Ovum.Diameter,Mean.Incubation,Larval.Growth))

# quickly see summaries for the new df fishcluster
for (i in 1:ncol(fishcluster)) {
  print(colnames(fishcluster)[i]) 
  print(summary(fishcluster[,i]))
}

# count NA's
nacount(fishcluster)

fishcluster4 <- select(fishinfo, c(Common.Name, Swim.Factor, Shape.Factor,Maximum.Literature.Length, Trophic.Guild, Length.at.Maturity, Maximum.Age, Age.at.Maturity, Mean.Fecundity, Mean.Ovum.Diameter, Parental.Care, R.Guild1:F.Guild3))

# make a dataframe of fishcluster with all the NAs removed, *** NOTE which "fishcluster" is used
fishclustercomplete <- fishcluster4[complete.cases(fishcluster4),]
#save(fishclustercomplete, file = "fishclustercomplete.Rda")
fishclustercomplete$Common.Name <- as.character(fishclustercomplete$Common.Name)

#Update fish names to current taxonomy (based on Google searches) and correct spelling errors.
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Lampetra appendix"] <- "Lethenteron appendix"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Lepisosteus spatula"] <- "Atractosteus spatula"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Notropis amblops"] <- "Hybopsis amblops"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                        "Hypopthalmichthys nobilis"] <- "Hypophthalmichthys nobilis"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Notropis hubbsi"] <- "Pteronotropis hubbsi"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Ammocrypta asprella"] <- "Crystallaria asprella"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Etheostoma proelaire"] <- "Etheostoma proeliare"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Myoxocephalus thompsoni"] <- "Myoxocephalus thompsonii"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Cottus bairdi"] <- "Cottus bairdii"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Phoxinus eos"] <- "Chrosomus eos"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Notropis amnis"] <- "Hybopsis amnis"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Notropis fumeus"] <- "Lythrurus fumeus"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Chologaster agassizi"] <- "Forbesichthys agassizii"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Percina uranidie"] <- "Percina uranidea"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Sander canadense"] <- "Sander canadensis"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Phoxinus erythrogaster"] <- "Chrosomus erythrogaster"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                      "Hypopthalmichthys molitrix"] <- "Hypophthalmichthys molitrix"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Sander vitreum"] <- "Sander vitreus"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Catostomus commersoni"] <- "Catostomus commersonii"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Moxostoma duquesnei"] <- "Moxostoma duquesnii"
fishinfo$Scientific.Name[fishinfo$Scientific.Name == 
                           "Notropis buccatus"] <- "Ericymba buccata"


library(rfishbase)
fb <- fishbase
head(fishinfo$Scientific.Name)
str(fishinfo$Scientific.Name)
length(fishinfo$Scientific.Name)
#it's a character vector. Good.
sciname <- paste(fb$Genus, fb$Species)
head(sciname)
index <- fishinfo$Scientific.Name %in% sciname
head(index)
fishcorrect <- fishinfo[index,]
dim(fishinfo)
dim(fishcorrect)
#206 species left

morpho <- morphometrics(species_list = fishcorrect$Scientific.Name)
#50 or more
length(unique(morpho$sciname))
#end up with 179 species

badfish <- fishinfo[!index,]
unique(badfish$Scientific.Name)













