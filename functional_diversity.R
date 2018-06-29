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
funcdiv <- select(pool8.fish, c(fishcode, barcode, snag, stratum_name, stratum.y, catch))

# remove rows where catch was <1
funcdiv <- filter(funcdiv, catch >=1)
filter(funcdiv, catch>1)

# repeat all the rows "catch" number of times
funcdiv2 <- funcdiv[rep(rownames(funcdiv), funcdiv$catch), ]
funcdiv2 <- select(funcdiv2, -c(catch))

# rename Fishcode so the 2 datasets are easier to merge
colnames(funcdiv2)[1] <- "Fishcode"
head(funcdiv2)

# select the colums that we will use for our regression 
fishinfo2 <- select(fishinfo, c(Fishcode, Common.Name, Scientific.Name.Current, Trophic.Guild, R.Guild1:F.Guild3, Parental.Care))
fishinfo2$Parental.Care <- factor(fishinfo2$Parental.Care)
fishinfo2$Trophic.Guild <- factor(fishinfo2$Trophic.Guild,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Heribivore", "Omnivore", "General Invertivore", 
                               "Benthic Invertivore", "Piscivore", "Planktivore"))
head(fishinfo2)
levels(fishinfo2$Trophic.Guild)

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
