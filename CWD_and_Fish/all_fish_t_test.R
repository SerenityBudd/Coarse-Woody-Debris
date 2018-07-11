source("libraries.R")

# load in the data
load("data/fishinfo2.Rda")
load("data/ltrmfishdat.Rda")


# make snag a factor
ltrmfishdat$snag <- factor(ltrmfishdat$snag,
                          levels = c(0,1),
                          labels = c("No", "Yes"))


# create a working dataframe with the cols we need
funcdiv <- select(ltrmfishdat, c(fishcode, barcode, snag, catch))


# remove rows where catch was <1
funcdiv <- filter(funcdiv, catch >=1)


# repeat all the rows "catch" number of times
funcdiv2 <- funcdiv[rep(rownames(funcdiv), funcdiv$catch), ]
funcdiv2 <- select(funcdiv2, -c(catch))


# rename Fishcode so the 2 datasets are easier to merge
colnames(funcdiv2)[1] <- "Fishcode"
head(funcdiv2)


# make fishcode a character vector so the matrices will merge
funcdiv2$Fishcode <- as.character(funcdiv2$Fishcode)
fishinfo2$Fishcode <- as.character(fishinfo2$Fishcode)


# merge the df 
funcdiv3.all <- left_join(funcdiv2, fishinfo2, by = "Fishcode")
#save(funcdiv3.all, file = "data/funcdiv3.all.Rda")


str(funcdiv3.all)
head(funcdiv3.all)
dim(funcdiv3.all)

#for (i in 1:ncol(funcdiv3.all)) {
#  print(colnames(funcdiv3.all)[i]) 
#  print(summary(funcdiv3.all[,i]))
#}




#######################################################
## DATA CLEANING

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx.all <- funcdiv3.all %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx.all)[3] <- "Num_Fish_per_Species"


## create a dataframe specifying the number of unique species per barcode
##     barcode  Num_Species
##    -1201327       9
yyy.all <- xxx.all %>% group_by(barcode) %>% count(barcode)
colnames(yyy.all)[2] <- "Num_Species"


## create a dataframe specifying the total number of fish per barcode
##     barcode  Num_Fish
##    -1201327     22
zzz.all <- xxx.all %>% group_by(barcode) %>% count(barcode, wt = Num_Fish_per_Species)
colnames(zzz.all)[2] <- "Num_Fish"


#######################################################
## T TESTS

## create a richness dataframe to do a t test on, combinging snag and yyy.all
dt.richness.t.all <- left_join(yyy.all, select(funcdiv3.all, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in richness between sites with and without CWD
with(dt.richness.t.all, t.test(Num_Species~snag, alternative = "less")) 


## create an abundance dataframe to do a t test on, combining snag and zzz.all
dt.abund.t.all <- left_join(zzz.all, select(funcdiv3.all, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in abundance between sites with and without CWD
with(dt.abund.t.all, t.test(Num_Fish~snag, alternative = "less"))

