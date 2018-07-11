source("libraries.R")

# load in the data
load("data/fishinfo.Rda")
load("data/ltrmfishdat.Rda")
load("data/strata.Rda")

# create a pool8 df using all of the fish in both fishinfo and fishdat
pool8.fish <- ltrmfishdat %>%
  filter(pool == "08") %>% droplevels()
pool8.fish <- left_join(pool8.fish, strata, by = "barcode")
pool8.fish$snag <- factor(pool8.fish$snag,
                          levels = c(0,1),
                          labels = c("No", "Yes"))

# create a working dataframe with the cols we need
funcdiv <- select(pool8.fish, c(fishcode, barcode, snag, stratum_name, stratum.y, length, weight, grp_wdth, gear, effmin, catch))

# remove rows where catch was <1
funcdiv <- filter(funcdiv, catch >=1)

# repeat all the rows "catch" number of times
funcdiv2 <- funcdiv[rep(rownames(funcdiv), funcdiv$catch), ]
funcdiv2 <- select(funcdiv2, -c(catch))

# rename Fishcode so the 2 datasets are easier to merge
colnames(funcdiv2)[1] <- "Fishcode"
head(funcdiv2)

# select the colums that we will use for our regression 
fishinfo2 <- select(fishinfo, c(Fishcode, Common.Name, Scientific.Name.Current, Swim.Factor:Turbidity.Tolerance, Trophic.Guild, R.Guild1:F.Guild3,Mean.Fecundity, Mean.Ovum.Diameter, Spawning.Duration,Spawning.Bouts,Age.at.Maturity:Maximum.Age))
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
funcdiv3 <- left_join(funcdiv2, fishinfo2, by = "Fishcode")
#save(funcdiv3, file = "data/funcdiv3.Rda")

str(funcdiv3)
head(funcdiv3)

#for (i in 1:ncol(funcdiv3)) {
#  print(colnames(funcdiv3)[i]) 
#  print(summary(funcdiv3[,i]))
#}


