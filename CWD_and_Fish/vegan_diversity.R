source("libraries.R")
library(vegan)
load("data/funcdiv3.Rda")

################################################
## creating a dataframe

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx <- funcdiv3 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx)[3] <- "Num_Fish_per_Species"


## spread the dataframe so each col is a fishcode
aaa <- spread(data = xxx, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa[is.na(aaa)] <- 0
## make the tibble a dataframe
aaa <- data.frame(aaa)


## make a dataframe that includes barcode/snag
dt.plot <- left_join(aaa, select(funcdiv3, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot <- dt.plot[complete.cases(dt.plot),]


## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb <- select(dt.plot, -c(barcode, snag))


################################################
## SHANNON DIVERSITY

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon <- diversity(x = bbb, index = "shannon")


## create a column in the dataframe for the shannon div index
dt.plot$div_shan <- div_shannon


## plot the density of the shannon div index values
plot(density(dt.plot$div_shan))


## plot a boxplot of shannon div and snag
with(dt.plot, boxplot(div_shan~snag))


## run a t-test on shannon div and snag
with(dt.plot, t.test(div_shan~snag, alternative = "less"))


## make vectors of the shannon div indices for snag vs no snag
x <- unlist(dt.plot %>% filter(snag == "Yes") %>% select(div_shan))
y <- unlist(dt.plot %>% filter(snag == "No") %>% select(div_shan))


## plot the densities of the vectors above
## shannon diversity of sites w/CWD
plot(density(x),col = "red")
## shannon diversity of sites w/o CWD
lines(density(y))
## add the lines for the values that the shannon indices usually fall under
abline(v = c(1.5, 3), col = "blue")


################################################
## SIIMPSON DIVERSITY

## use 'vegan' to calculate the simpson diversity index for each barcode
div_simpson <- diversity(x = bbb, index = "simpson")


## create a column in the dataframe for the simpson div index
dt.plot$div_simp <- div_simpson


## plot the density of the simpson div index values
plot(density(dt.plot$div_simp))


## plot a boxplot of simpson div and snag
with(dt.plot, boxplot(div_simp~snag))


## run a t-test on simpson div and snag
with(dt.plot, t.test(div_simp~snag, alternative = "less"))


## make vectors of the simpson div indices for snag vs no snag
x <- unlist(dt.plot %>% filter(snag == "Yes") %>% select(div_simp))
y <- unlist(dt.plot %>% filter(snag == "No") %>% select(div_simp))


## plot the densities of the vectors above
## simpson diversity of sites w CWD
plot(density(x),col = "red")
## simpson diversity of sites w/o CWD
lines(density(y))


################################################
## using fisher, prestiondistr, radfit
k <- sample(nrow(bbb), 1)

fisher.alpha(x = bbb[k,])

fishf <- fisherfit(bbb[k,])
fishf
plot(fishf)


prestondistr(bbb[k,])
plot(prestondistr(bbb[k,]))


radfit(x = bbb[k,])
plot(radfit(x = bbb[k,]))
