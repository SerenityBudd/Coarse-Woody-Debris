source("libraries.R")

# load in the data
load("data/funcdiv4.8.13.Rda")


#######################################################
## DATA CLEANING

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx.all <- funcdiv4.8.13 %>% group_by(barcode) %>% count(Fishcode)
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
dt.richness.t.all <- left_join(yyy.all, select(funcdiv4.8.13, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in richness between sites with and without CWD
with(dt.richness.t.all, t.test(Num_Species~snag, alternative = "less")) 


## create an abundance dataframe to do a t test on, combining snag and zzz.all
dt.abund.t.all <- left_join(zzz.all, select(funcdiv4.8.13, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in abundance between sites with and without CWD
with(dt.abund.t.all, t.test(Num_Fish~snag, alternative = "less"))

