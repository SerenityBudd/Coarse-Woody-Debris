source("libraries.R")
load("data/funcdiv4.Rda")

################################################
## creating a dataframe

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx4 <- funcdiv4 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx4)[3] <- "Num_Fish_per_Species"


## spread the dataframe so each col is a fishcode
aaa4 <- spread(data = xxx4, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa4[is.na(aaa4)] <- 0
## make the tibble a dataframe
aaa4 <- data.frame(aaa4)


## make a dataframe that includes barcode/snag
dt.plot4 <- left_join(aaa4, select(funcdiv4, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot4 <- dt.plot4[complete.cases(dt.plot4),]


## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb4 <- select(dt.plot4, -c(barcode, snag))


################################################
## SHANNON DIVERSITY

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon <- diversity(x = bbb4, index = "shannon", base = 2)


## create a column in the dataframe for the shannon div index
dt.plot4$div_shan <- div_shannon


## plot a boxplot of shannon div and snag
with(dt.plot4, boxplot(div_shan~snag))


## run a t-test on shannon div and snag
with(dt.plot4, t.test(div_shan~snag, alternative = "less"))


## plot the densities of the vectors above
ggplot(data = dt.plot4, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  scale_fill_manual(values = c("blue", "red")) + 
  geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
  xlab("Shannon–Weaver Diversity Index") +
  ylab("Density") +
  ggtitle("Density Plot of Shannon-Weaver Index at Pool 4 Sites")
################################################


