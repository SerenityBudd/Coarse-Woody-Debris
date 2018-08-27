source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Shannon Weaver Diversity

## filter the dataframe by pool
funcy <- funcdiv4.8.13 
## create a dataframe specifying the number of fish per species per barcode
xxx <- funcy %>%
  group_by(barcode) %>% count(Fishcode)
colnames(xxx)[3] <- "Num_Fish_per_Species"

## spread the dataframe so each col is a fishcode
aaa <- spread(data = xxx, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa[is.na(aaa)] <- 0
## make the tibble a dataframe
aaa <- data.frame(aaa)

## make a dataframe that includes barcode/snag
dt.plot <- left_join(aaa, select(funcy, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot <- dt.plot[complete.cases(dt.plot),]

## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb <- select(dt.plot, -c(barcode, snag))

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon <- diversity(x = bbb, index = "shannon", base = 2)

## create a column in the dataframe for the shannon div index
dt.plot$div_shan <- div_shannon

divstats <- 
  group_by(dt.plot, snag) %>%
    summarise(
      count = n(),
      mean = mean(div_shan, na.rm = TRUE),
      sd = sd(div_shan, na.rm = TRUE)
    )

## run a t-test on shannon div and snag
t.div <- 
  with(dt.plot, t.test(div_shan~snag, alternative = "less"))

## plot the densities of the vectors above
ggplot(data = dt.plot, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
  xlab("Shannon–Weaver Diversity Index") +
  ylab("Density") +
  ggtitle(paste("Density plot of Shannon-Weaver index at all pools"))