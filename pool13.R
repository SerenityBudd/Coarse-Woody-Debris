source("libraries.R")
load("data/funcdiv13.Rda")

################################################
## creating a dataframe

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx13 <- funcdiv13 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx13)[3] <- "Num_Fish_per_Species"


## spread the dataframe so each col is a fishcode
aaa13 <- spread(data = xxx13, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa13[is.na(aaa13)] <- 0
## make the tibble a dataframe
aaa13 <- data.frame(aaa13)


## make a dataframe that includes barcode/snag
dt.plot13 <- left_join(aaa13, select(funcdiv13, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot13 <- dt.plot13[complete.cases(dt.plot13),]


## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb13 <- select(dt.plot13, -c(barcode, snag))


################################################
## SHANNON DIVERSITY

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon <- diversity(x = bbb13, index = "shannon", base = 2)


## create a column in the dataframe for the shannon div index
dt.plot13$div_shan <- div_shannon


## plot a boxplot of shannon div and snag
with(dt.plot13, boxplot(div_shan~snag))


## run a t-test on shannon div and snag
with(dt.plot13, t.test(div_shan~snag, alternative = "less"))


## plot the densities of the vectors above
ggplot(data = dt.plot13, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  scale_fill_manual(values = c("blue", "red")) + 
  geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
  xlab("Shannon–Weaver Diversity Index") +
  ylab("Density") +
  ggtitle("Density Plot of Shannon-Weaver Index at Pool 13 Sites")
################################################


