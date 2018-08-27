source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Shannon Wiener Diversity

## filter the dataframe by pool
funcy13 <- funcdiv4.8.13 %>% 
  filter(pool %in% "13") 
## create a dataframe specifying the number of fish per species per barcode
xxx13 <- funcy13 %>%
  group_by(barcode) %>% count(Fishcode)
colnames(xxx13)[3] <- "Num_Fish_per_Species"

## spread the dataframe so each col is a fishcode
aaa13 <- spread(data = xxx13, value = Num_Fish_per_Species, Fishcode) %>%
  as.data.frame()
## replace the NAs with zero
aaa13[is.na(aaa13)] <- 0

## make a dataframe that includes barcode/snag
dt.plot13 <- left_join(aaa13, select(funcy13, c(barcode, snag)), by = "barcode") %>% distinct 

## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb13 <- select(dt.plot13, -c(barcode, snag))

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon13 <- diversity(x = bbb13, index = "shannon", base = 2)

## create a column in the dataframe for the shannon div index
dt.plot13$div_shan <- div_shannon13

## summary statistics
div13stats <- group_by(dt.plot13, snag) %>%
  summarise(
    count = n(),
    mean = mean(div_shan, na.rm = TRUE),
    sd = sd(div_shan, na.rm = TRUE)
  )

## run a t-test on shannon div and snag
t.div13 <- with(dt.plot13, t.test(div_shan~snag, alternative = "less"))

## plot the densities of the vectors above
ggplot(data = dt.plot13, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  xlab("Shannon–Wiener Diversity Index") +
  ylab("Density") +
  ggtitle(paste("Density Plot of Shannon-Wiener Index at Pool 13 Sites"))
