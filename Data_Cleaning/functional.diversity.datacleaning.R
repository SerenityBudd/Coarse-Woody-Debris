source("libraries.R")

# load in the data
load("data/fishinfo.Rda")
load("data/ltrmfishdat.Rda")


# create a working dataframe with the cols we need
funcdiv <- select(ltrmfishdat, c(fishcode, barcode, snag, snag01, stratum, stratum_name, length, weight, grp_wdth, effmin,secchi,temp, depth, current, pool, catch))

# remove rows where catch was <1
funcdiv <- filter(funcdiv, catch >=1)

# repeat all the rows "catch" number of times
funcdiv2 <- funcdiv[rep(rownames(funcdiv), funcdiv$catch), ]
funcdiv2 <- select(funcdiv2, -c(catch))

# rename Fishcode so the 2 datasets are easier to merge
colnames(funcdiv2)[1] <- "Fishcode"
head(funcdiv2)

# select the colums that we will use for our regression 
fishinfo2 <- select(fishinfo, c(Fishcode, Common.Name, Family.Name, Scientific.Name.Current, Swim.Factor:Turbidity.Tolerance, Trophic.Guild, R.Guild1:F.Guild3,Mean.Fecundity, Mean.Ovum.Diameter, Spawning.Duration,Spawning.Bouts,Age.at.Maturity:Maximum.Age, Native))
fishinfo2$Trophic.Guild <- factor(fishinfo2$Trophic.Guild,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Herbivore", "Omnivore", "General Invertivore", 
                               "Benthic Invertivore", "Piscivore", "Planktivore"))
fishinfo2$Current.Preference <- factor(fishinfo2$Current.Preference,
                                       levels = c(1,2,3,4),
                                       labels = c("Fast", "Moderate", "Slow-none", 
                                                  "General"))
fishinfo2$Substrate.Preference <- factor(fishinfo2$Substrate.Preference,
                                       levels = c(1,2,3,4,5,6,7,8),
                                       labels = c("Cobble", "Gravel", "Sand", 
                                                  "Silt", "General",	"Vegetation", "Structure", "Pelagic"))
fishinfo2$Spawning.Substrate <- factor(fishinfo2$Spawning.Substrate,
                                         levels = c(1,2,3,4,5,6,7,8),
                                         labels = c("Cobble", "Gravel", "Sand", 
                                                    "Silt", "General",	"Vegetation", "Structure", "Pelagic"))
fishinfo2$Turbidity.Tolerance <- factor(fishinfo2$Turbidity.Tolerance,
                                       levels = c(1,2,3),
                                       labels = c("High", "Medium", "Low"))
fishinfo2$Silt.Tolerance <- factor(fishinfo2$Silt.Tolerance,
                                        levels = c(1,2,3),
                                        labels = c("High", "Medium", "Low"))

str(fishinfo2)
head(fishinfo2)
#save(fishinfo2, file = "data/fishinfo2.Rda")


#for (i in 1:ncol(fishinfo2)) {
#  print(colnames(fishinfo2)[i]) 
#  print(summary(fishinfo2[,i]))
#}

# make fishcode a character vector so the matrices will merge
funcdiv2$Fishcode <- as.character(funcdiv2$Fishcode)
fishinfo2$Fishcode <- as.character(fishinfo2$Fishcode)

# merge the df 
funcdiv.all <- left_join(funcdiv2, fishinfo2, by = "Fishcode")
#save(funcdiv.all, file = "data/funcdiv.all.Rda")

str(funcdiv.all)
head(funcdiv.all)
dim(funcdiv.all)

#for (i in 1:ncol(funcdiv3)) {
#  print(colnames(funcdiv3)[i]) 
#  print(summary(funcdiv3[,i]))
#}

# create a pool8 df using all of the fish in both fishinfo and fishdat
funcdiv4 <- funcdiv.all %>%
  filter(pool == "04") %>% droplevels()
#save(funcdiv4, file = "data/funcdiv4.Rda")

funcdiv8 <- funcdiv.all %>%
  filter(pool == "08") %>% droplevels()
#save(funcdiv8, file = "data/funcdiv8.Rda")

funcdiv13 <- funcdiv.all %>%
  filter(pool == "13") %>% droplevels()
#save(funcdiv13, file = "data/funcdiv13.Rda")

funcdiv26 <- funcdiv.all %>%
  filter(pool == "26") %>% droplevels()
#save(funcdiv26, file = "data/funcdiv26.Rda")

funcdivLG <- funcdiv.all %>%
  filter(pool == "LG") %>% droplevels()
#save(funcdivLG, file = "data/funcdivLG.Rda")

funcdivOR <- funcdiv.all %>%
  filter(pool == "OR") %>% droplevels()
#save(funcdivOR, file = "data/funcdivOR.Rda")

# create a dataframe for the top 3 pools
funcdiv4.8.13 <- funcdiv.all %>%
  filter(pool == "04" | pool == "08" | pool == "13") %>%
  droplevels()
#save(funcdiv4.8.13, file = "data/funcdiv4.8.13.Rda")

#POOL/REACH
#Alphanumeric code for the LTRMP study reach or pool number:
#  04 = Pool 4, 26 = Pool 26
#  08 = Pool 8, LG = La Grange Pool, Illinois River
#  13 = Pool 13, OR = Open Mississippi River

#######################################################
fishtraits_new <- read.csv("data/new_fishtraits.csv")
#save(fishtraits_new, file = "data/fishtraits_new.Rda")

funcdiv2.new <- 
  left_join(funcdiv2, select(fishinfo2, c(Fishcode, Common.Name)),  by = "Fishcode")


funcdiv2.new$Common.Name <- as.character(funcdiv2.new$Common.Name)
fishtraits_new$Common.Name <- as.character(fishtraits_new$Common.Name)

setdiff(fishtraits_new[,"Common.Name"], funcdiv2.new[,"Common.Name"])
## Mosquitofish = Western mosquitofish, Northern hogsucker = Northern hog sucker, Rockbass = Rock bass, Sicklefin Chub = Sicklefin chub

fishtraits_new$Common.Name[fishtraits_new$Common.Name == "Mosquitofish"] <- "Western mosquitofish"
fishtraits_new$Common.Name[fishtraits_new$Common.Name == "Northern hogsucker"] <- "Northern hog sucker"
fishtraits_new$Common.Name[fishtraits_new$Common.Name == "Rockbass"] <- "Rock bass"
fishtraits_new$Common.Name[fishtraits_new$Common.Name == "Sicklefin Chub"] <- "Sicklefin chub"


# the fishcodes in both the LTRM data and fish traits
inters3 <- intersect(unique(funcdiv2.new[,"Common.Name"]), fishtraits_new[,"Common.Name"])


dim(filter(funcdiv2.new, Common.Name %in% setdiff(funcdiv2.new[,"Common.Name"], fishtraits_new[,"Common.Name"])))
## would lose out on 11955 rows out of 1543161


dim(filter(funcdiv2.new, Common.Name %in% inters3))
## there would be 1531206 rows left


fishtraits_new$Trophic.Level <- factor(fishtraits_new$Trophic.Level)

fishtraits.new <- fishtraits_new %>%
  filter(Common.Name %in% inters3) %>%
  separate(col = Trophic.Level, into = c("Trophic.Number","Trophic.Guild"), sep = "-", remove = T) %>%
  filter(!Trophic.Number == "" & !Spawning.Group == "") %>%
  droplevels()

fishtraits.new$Trophic.Number <- factor(fishtraits.new$Trophic.Number)
fishtraits.new$Trophic.Guild <- factor(fishtraits.new$Trophic.Guild)

levels(fishtraits.new$Spawning.Group)
levels(fishtraits.new$Trophic.Number)
levels(fishtraits.new$Trophic.Guild)

sum(is.na(fishtraits.new))
#save(fishtraits.new, file = "data/fishtraits.new.Rda")

funcdiv4.8.13.new <- 
  left_join(funcdiv2.new, fishtraits.new, by = "Common.Name") %>%
  filter(pool == "04" | pool == "08" | pool == "13") %>%
  droplevels()
#save(funcdiv4.8.13.new, file = "data/funcdiv4.8.13.new.Rda")

