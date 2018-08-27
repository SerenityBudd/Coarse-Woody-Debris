source("libraries.R")

# load in the data
load("data/fishinfo.Rda")
load("data/ltrmfishdat.Rda")


# create a working dataframe with the cols we need
funcdiv <- select(ltrmfishdat, c(fishcode, barcode, snag, snag01, stratum, stratum_name, length, weight, grp_wdth, effmin,secchi,temp, depth, current, pool, catch, sdate, wingdyke))

# remove rows where catch was <1
funcdiv <- filter(funcdiv, catch >=1)

# repeat all the rows "catch" number of times
funcdiv2 <- funcdiv[rep(rownames(funcdiv), funcdiv$catch), ]
# remove the column "catch"
funcdiv2 <- select(funcdiv2, -c(catch))

# rename Fishcode so the 2 datasets are easier to merge
colnames(funcdiv2)[1] <- "Fishcode"
head(funcdiv2)

# select the colums that we will use for our regression 
fishinfo2 <- select(fishinfo, c(Fishcode, Common.Name, Family.Name, Scientific.Name.Current, Swim.Factor:Turbidity.Tolerance, Trophic.Guild, R.Guild1:F.Guild3,Mean.Fecundity, Mean.Ovum.Diameter, Spawning.Duration,Spawning.Bouts,Age.at.Maturity:Maximum.Age, Native))

# make Trophic.Guild a factor with clarified names
fishinfo2$Trophic.Guild <- factor(fishinfo2$Trophic.Guild,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Herbivore", "Omnivore", "General Invertivore", 
                               "Benthic Invertivore", "Piscivore", "Planktivore"))

# make Current.Preference a factor with clarified names
fishinfo2$Current.Preference <- factor(fishinfo2$Current.Preference,
                                       levels = c(1,2,3,4),
                                       labels = c("Fast", "Moderate", "Slow-none", 
                                                  "General"))

# make Substrate.Preference a factor with clarified names
fishinfo2$Substrate.Preference <- factor(fishinfo2$Substrate.Preference,
                                       levels = c(1,2,3,4,5,6,7,8),
                                       labels = c("Cobble", "Gravel", "Sand", 
                                                  "Silt", "General",	"Vegetation", "Structure", "Pelagic"))

# make Spawning.Substrate a factor with clarified names
fishinfo2$Spawning.Substrate <- factor(fishinfo2$Spawning.Substrate,
                                         levels = c(1,2,3,4,5,6,7,8),
                                         labels = c("Cobble", "Gravel", "Sand", 
                                                    "Silt", "General",	"Vegetation", "Structure", "Pelagic"))

# make Turbidity.Tolerance a factor with clarified names
fishinfo2$Turbidity.Tolerance <- factor(fishinfo2$Turbidity.Tolerance,
                                       levels = c(1,2,3),
                                       labels = c("High", "Medium", "Low"))

# make Silt.Tolerance a factor with clarified names
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

# create a pool4 df
funcdiv4 <- funcdiv.all %>%
  filter(pool == "04") %>% droplevels()
#save(funcdiv4, file = "data/funcdiv4.Rda")

# create a pool8 df
funcdiv8 <- funcdiv.all %>%
  filter(pool == "08") %>% droplevels()
#save(funcdiv8, file = "data/funcdiv8.Rda")

# create a pool13 df
funcdiv13 <- funcdiv.all %>%
  filter(pool == "13") %>% droplevels()
#save(funcdiv13, file = "data/funcdiv13.Rda")

# create a dataframe for the top 3 pools
funcdiv4.8.13 <- funcdiv.all
#save(funcdiv4.8.13, file = "data/funcdiv4.8.13.Rda")
