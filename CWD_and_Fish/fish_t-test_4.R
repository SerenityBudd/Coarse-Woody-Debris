source("libraries.R")
load("data/funcdiv4.Rda")


#######################################################
## DATA CLEANING

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx4 <- funcdiv4 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx4)[3] <- "Num_Fish_per_Species"


## create a dataframe specifying the number of unique species per barcode
##     barcode  Num_Species
##    -1201327       9
yyy4 <- xxx4 %>% group_by(barcode) %>% count(barcode)
colnames(yyy4)[2] <- "Num_Species"


## create a dataframe specifying the total number of fish per barcode
##     barcode  Num_Fish
##    -1201327     22
zzz4 <- xxx4 %>% group_by(barcode) %>% count(barcode, wt = Num_Fish_per_Species)
colnames(zzz4)[2] <- "Num_Fish"


#######################################################
## RICHNESS

##########################
## t-test

## create a richness dataframe to do a t test on, combinging snag and yyy4
dt.richness.t <- left_join(yyy4, select(funcdiv4, c(barcode, snag)), by = "barcode") %>% distinct

rich4stats <- 
  group_by(dt.richness.t, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Species, na.rm = TRUE),
    sd = sd(Num_Species, na.rm = TRUE)
  )

## there is a signigficant diff in richness between sites with and without CWD
t.rich4 <- with(dt.richness.t, t.test(Num_Species~snag, alternative = "less")) 

##########################
## plotting the data 
with(dt.richness.t, boxplot(Num_Species~snag))



#######################################################
## ABUNDANCE

##########################
## t-test

## create an abundance dataframe to do a t test on, combining snag and zzz4
dt.abund.t <- left_join(zzz4, select(funcdiv4, c(barcode, snag)), by = "barcode") %>% distinct

abund4stats <- 
  group_by(dt.abund.t, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Fish, na.rm = TRUE),
    sd = sd(Num_Fish, na.rm = TRUE)
  )

## there is NOT a sig diff in abundance between sites with and without CWD
t.abund4 <- with(dt.abund.t, t.test(Num_Fish~snag, alternative = "less"))

## when the assumption of normality is not present, there IS a sig diff in abundance between sites with and without CWD
with(dt.abund.t, wilcox.test(Num_Fish~snag, alternative = "less"))


## this means i can attribute it to the outliers
with(filter(dt.abund.t, Num_Fish < 2500), t.test(Num_Fish~snag, alternative = "less"))


##########################
## plotting the data 
with(dt.abund.t, boxplot(Num_Fish~snag, ylim = c(0,400)))

