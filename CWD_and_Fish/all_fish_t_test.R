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
## RICHNESS

##########################
## t-test

## create a richness dataframe to do a t test on, combinging snag and yyy.all
dt.richness.all <- left_join(yyy.all, select(funcdiv4.8.13, c(barcode, snag)), by = "barcode") %>% distinct


## summary statistics 
richstats <- 
  group_by(dt.richness.all, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Species, na.rm = TRUE),
    sd = sd(Num_Species, na.rm = TRUE)
  )


## the sample size is large enough (n > 30), we ignore the distribution of the data and use the t-test

t.rich <- with(dt.richness.all, t.test(Num_Species~snag, alternative = "less")) 
## there is a signigficant diff in richness between sites with and without CWD
#with(dt.richness.all, wilcox.test(Num_Species~snag, alternative = "less")) 
##########################
## plotting the data 

## boxplot for species richness ~ snag
ggboxplot(dt.richness.all, x = "snag", y = "Num_Species", 
          color = "snag", palette = c("#00AFBB", "#E7B800"),
          ylab = "Species Richness", xlab = "Presence of Large Wood")

## density plots for species richness ~ snag 
ggplot(data = dt.richness.all, aes(Num_Species)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  xlab("Species Richness") +
  ylab("Density") +
  ggtitle(paste("Density Plot of Species Richness at all Pools"))


#######################################################
## ABUNDANCE

##########################
## t-test

## create an abundance dataframe to do a t test on, combining snag and zzz.all
dt.abund.all <- left_join(zzz.all, select(funcdiv4.8.13, c(barcode, snag)), by = "barcode") %>% distinct


## summary statistics 
abundstats <- 
  group_by(dt.abund.all, snag) %>%
  summarise(
    count = n(),
    mean = mean(Num_Fish, na.rm = TRUE),
    sd = sd(Num_Fish, na.rm = TRUE)
  )

## the sample size is large enough (n > 30), we ignore the distribution of the data and use the t-test

t.abund <- with(dt.abund.all, t.test(Num_Fish~snag, alternative = "less"))
## there is a signigficant diff in abundance between sites with and without CWD
#with(dt.abund.all, wilcox.test(Num_Fish~snag, alternative = "less"))


##########################
## plotting the data 

## boxplots for species abundance ~ snag
ggboxplot(dt.abund.all, x = "snag", y = "Num_Fish", 
          color = "snag", palette = c("#00AFBB", "#E7B800"),
          ylab = "Species Abundance", xlab = "Presence of Large Wood")

## density plots for species abundance ~ snag
ggplot(data = dt.abund.all, aes(Num_Fish)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  xlab("Species Abundance") +
  ylab("Density") +
  ggtitle(paste("Density Plot of Species Abundance at all Pools"))


#######################################################

