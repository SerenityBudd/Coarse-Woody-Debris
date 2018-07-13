source("libraries.R")
load("data/funcdiv8.Rda")

################################################
## creating a dataframe

## create a dataframe specifying the number of fish per species per barcode
##     barcode Fishcode Num_Fish_per_Species
##    -1201327   RVRH           6
xxx8 <- funcdiv8 %>% group_by(barcode) %>% count(Fishcode)
colnames(xxx8)[3] <- "Num_Fish_per_Species"

## spread the dataframe so each col is a fishcode
aaa8 <- spread(data = xxx8, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa8[is.na(aaa8)] <- 0
## make the tibble a dataframe
aaa8 <- data.frame(aaa8)


## make a dataframe that includes barcode/snag
dt.plot8 <- left_join(aaa8, select(funcdiv8, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot8 <- dt.plot8[complete.cases(dt.plot8),]


## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb8 <- select(dt.plot8, -c(barcode, snag))


################################################
## SHANNON DIVERSITY

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon <- diversity(x = bbb8, index = "shannon", base = 2)


## create a column in the dataframe for the shannon div index
dt.plot8$div_shan <- div_shannon


## plot a boxplot of shannon div and snag
with(dt.plot8, boxplot(div_shan~snag))


## run a t-test on shannon div and snag
with(dt.plot8, t.test(div_shan~snag, alternative = "less"))


## plot the densities of the vectors above
ggplot(data = dt.plot8, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  scale_fill_manual(values = c("blue", "red")) + 
  geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
  xlab("Shannon–Weaver Diversity Index") +
  ylab("Density") +
  ggtitle("Density Plot of Shannon-Weaver Index at Pool 8 Sites")


################################################
## Rank abundance curve

RACplot <- function(funcdiv, i) {
  xxx <- funcdiv %>% group_by(stratum_name, snag) %>% count(Fishcode)
  rac <- spread(data = xxx, value = n, Fishcode) %>% data.frame(.)
  rac[is.na(rac)] <- 0
  
  raddy <- as.rad(x = rac[i,-c(1,2)])
  rad.df <- data.frame(raddy[1:length(raddy)])
  rad.df$Fishcode <- rownames(rad.df)
  colnames(rad.df) <- c("Abundance", "Fishcode")
  
  ggplot(data = rad.df, aes(reorder(Fishcode, - Abundance), Abundance, group = 1)) + 
    geom_point(stat = "identity",color = "darkblue") +
    scale_y_continuous(trans='log10', breaks= c(10^c(0:5))) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Fishcode") +
    ylab("Abundance (Log10 Scale)") 
}

RACplot(funcdiv8, 1)


################################################
## Abundance plot 

ABUNDplot <- function(funcdiv, Snag, Stratum) {
  xplot <- 
    funcdiv %>% group_by(stratum_name, snag) %>% 
    count(Fishcode) %>%
    filter(snag == Snag
           & stratum_name == Stratum)

  ggplot(data = xplot, aes(x = reorder(Fishcode, -n), n, group = "group")) +
    geom_point(stat = "identity", color = "darkblue") +
    geom_line() +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Fishcode") +
    ylab("Abundance") 
}

ABUNDplot(funcdiv8, "No", "Backwater, Contiguous Shoreline")
