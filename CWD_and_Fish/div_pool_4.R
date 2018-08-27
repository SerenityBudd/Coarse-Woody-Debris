source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Shannon Weaver Diversity

## filter the dataframe by pool
funcy4 <- funcdiv4.8.13 %>% 
  filter(pool %in% "04") 
## create a dataframe specifying the number of fish per species per barcode
xxx4 <- funcy4 %>%
  group_by(barcode) %>% count(Fishcode)
colnames(xxx4)[3] <- "Num_Fish_per_Species"

## spread the dataframe so each col is a fishcode
aaa4 <- spread(data = xxx4, value = Num_Fish_per_Species, Fishcode) 
## replace the NAs with zero
aaa4[is.na(aaa4)] <- 0
## make the tibble a dataframe
aaa4 <- data.frame(aaa4)

## make a dataframe that includes barcode/snag
dt.plot4 <- left_join(aaa4, select(funcy4, c(barcode, snag)), by = "barcode") %>% distinct 
dt.plot4 <- dt.plot4[complete.cases(dt.plot4),]

## make a dataframe to do analysis on, leaves out barcodes and snag out
bbb4 <- select(dt.plot4, -c(barcode, snag))

## use 'vegan' to calculate the shannon diversity index for each barcode
div_shannon4 <- diversity(x = bbb4, index = "shannon", base = 2)

## create a column in the dataframe for the shannon div index
dt.plot4$div_shan <- div_shannon4

div4stats <- group_by(dt.plot4, snag) %>%
    summarise(
      count = n(),
      mean = mean(div_shan, na.rm = TRUE),
      sd = sd(div_shan, na.rm = TRUE)
    )

## run a t-test on shannon div and snag
t.div4 <- with(dt.plot4, t.test(div_shan~snag, alternative = "less"))

## plot the densities of the vectors above
ggplot(data = dt.plot4, aes(div_shan)) + 
  geom_density(aes(fill = snag), alpha = .3) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
  geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
  xlab("Shannon–Weaver Diversity Index") +
  ylab("Density") +
  ggtitle(paste("Density Plot of Shannon-Weaver Index at Pool 04 Sites"))
