source("libraries.R")

# load in the data
load("data/fishinfo.Rda")
load("data/ltrmfishdat.Rda")
load("data/strata.Rda")

# create a pool8 df using all of the fish in both fishinfo and fishdat
pool8.fish <- ltrmfishdat %>%
  filter(pool == "08") %>% droplevels()
pool8.fish <- left_join(pool8.fish, strata, by = "barcode")

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
head(fishinfo2)

#for (i in 1:ncol(fishinfo2)) {
#  print(colnames(fishinfo2)[i]) 
#  print(summary(fishinfo2[,i]))
#}

# make fishcode a character vector so the matrices will merge
funcdiv2$Fishcode <- as.character(funcdiv2$Fishcode)
fishinfo2$Fishcode <- as.character(fishinfo2$Fishcode)

# merge the df 
funcdiv3 <- left_join(funcdiv2, fishinfo2, by = "Fishcode")

str(funcdiv3)
head(funcdiv3)
View(funcdiv3)
