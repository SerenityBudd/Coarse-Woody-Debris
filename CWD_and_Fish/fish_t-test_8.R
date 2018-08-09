source("libraries.R")
load("data/funcdiv8.Rda")


#######################################################
## DATA CLEANING

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx <- funcdiv8 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx)[3] <- "Num_Fish_per_Species"


## create a dataframe specifying the number of unique species per barcode
##     barcode  Num_Species
##    -1201327       9
yyy <- xxx %>% group_by(barcode) %>% count(barcode)
colnames(yyy)[2] <- "Num_Species"


## create a dataframe specifying the total number of fish per barcode
##     barcode  Num_Fish
##    -1201327     22
zzz <- xxx %>% group_by(barcode) %>% count(barcode, wt = Num_Fish_per_Species)
colnames(zzz)[2] <- "Num_Fish"


#######################################################
## T TESTS

## create a richness dataframe to do a t test on, combinging snag and yyy
dt.richness.t <- left_join(yyy, select(funcdiv8, c(barcode, snag)), by = "barcode") %>% distinct

group_by(dt.richness.t, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Species, na.rm = TRUE),
    sd = sd(Num_Species, na.rm = TRUE)
  )

## there is a signigficant diff in richness between sites with and without CWD
with(dt.richness.t, t.test(Num_Species~snag, alternative = "less")) 


with(dt.richness.t, boxplot(Num_Species~snag))


## create an abundance dataframe to do a t test on, combining snag and zzz
dt.abund.t <- left_join(zzz, select(funcdiv8, c(barcode, snag)), by = "barcode") %>% distinct

group_by(dt.abund.t, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Fish, na.rm = TRUE),
    sd = sd(Num_Fish, na.rm = TRUE)
  )

## there is a signigficant diff in abundance between sites with and without CWD
with(dt.abund.t, t.test(Num_Fish~snag, alternative = "less"))


with(dt.abund.t, boxplot(Num_Fish~snag, ylim = c(0,400)))

