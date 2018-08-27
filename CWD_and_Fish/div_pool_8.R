source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Shannon Wiener Diversity

## filter the dataframe by pool
funcy8 <- funcdiv4.8.13 %>% 
  filter(pool %in% "08") 
## create a dataframe specifying the number of fish per species per barcode
xxx8 <- funcy8 %>%
  group_by(barcode) %>% count(Fishcode)
colnames(xxx8)[3] <- "Num_Fish_per_Species"

## spread the dataframe so each col is a fishcode
aaa8 <- spread(data = xxx8, value = Num_Fish_per_Species, Fishcode) %>%
  as.data.frame()
## replace the NAs with zero
aaa8[is.na(aaa8)] <- 0

## make a dataframe that includes barcode/snag
dt.plot8 <- left_join(aaa8, select(funcy8, c(barcode, snag)), by = "barcode") %>% distinct 

## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb8 <- select(dt.plot8, -c(barcode, snag))

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon8 <- diversity(x = bbb8, index = "shannon", base = 2)

## create a column in the dataframe for the shannon div index
dt.plot8$div_shan <- div_shannon8

## summary statistics
div8stats <- group_by(dt.plot8, snag) %>%
  summarise(
    count = n(),
    mean = mean(div_shan, na.rm = TRUE),
    sd = sd(div_shan, na.rm = TRUE)
  )

## run a t-test on shannon div and snag
t.div8 <- with(dt.plot8, t.test(div_shan~snag, alternative = "less"))

## plot the densities of the vectors above
ggplot(data = dt.plot8, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  xlab("Shannon–Wiener Diversity Index") +
  ylab("Density") +
  ggtitle(paste("Density plot of Shannon-Wiener index at pool 08 sites"))
