source("libraries.R")
load("data/funcdiv3.Rda")

####################################################
## DATA CLEANING
dt <- data.table(funcdiv3)
head(dt)

## Get count (N) of number of each species (common name) per site (barcode)
## Also get keep Tropic Guild and snag and statum name
dt2 <- 
  dt[ , .N, by = list(barcode, Trophic.Guild, snag, 
                           stratum_name, Common.Name)]
dim(dt2)


## Get count (N) of species per tropic guild by site using previous summary
dt3 <-
  dt2[ , .N, by = list(barcode, Trophic.Guild, snag, 
                      stratum_name)]
dim(dt3)


## Remove all NAs
dt4 <- dt3[ complete.cases(dt3), ]
dim(dt4)
summary(dt4)


####################################################
## INVESTIGATE THE DATA TO UNDERSTAND ERRORS
table(dt4$snag, dt4$Trophic.Guild)
table(dt4$snag, dt4$N)
table(dt4$N, dt4$Trophic.Guild)


## there are 36 observations of herbivore
sum(funcdiv3$Trophic.Guild == "Herbivore", na.rm = T)
summary(funcdiv3$Trophic.Guild)


## there are 35 observations of herbivore when all NAs are removed
funcdiv4 <- funcdiv3[complete.cases(select(funcdiv3, c(barcode, Trophic.Guild, snag, stratum_name, Common.Name))),]
summary(funcdiv4$Trophic.Guild)


## there are 22 unique barcodes with herbivores at them
fd5 <- funcdiv4 %>% select(c(barcode, Trophic.Guild, Common.Name)) %>% 
  filter(Trophic.Guild == "Herbivore")
length(unique(fd5$barcode))
## 21 of the fish are Highfin carpsucker, and the other 1 is Largescale stoneroller


####################################################
## ADD THE ZERO COUNTS TO THE DATA

## make variables for the number of time to rep
barcode.l <- length(unique(dt4$barcode))
Guild.l <- length(unique(dt4$Trophic.Guild))


## make the vectors of rep barcodes and trophic guilds
dt.c.1 <- sort(rep(unique(dt4$barcode), Guild.l))
dt.c.2 <- rep(unique(dt4$Trophic.Guild), barcode.l)


## make sure they are the same length
length(dt.c.1)
length(dt.c.2)


## combine them into a dt of the barcodes and trophic guilds
dt.c <- data.table(barcode = dt.c.1, Trophic.Guild = dt.c.2)


## make a working data frame of all the barcodes, snag and stratum
funcdiv.wrk <- funcdiv3 %>% select(c(barcode, snag, stratum_name)) %>%
  group_by(barcode) %>% summarise_all(funs(first))


## join those
dt.fin <- left_join(dt.c, funcdiv.wrk, by = "barcode")


## make the final data table
dt.final <- left_join(dt.fin, select(dt4, c(barcode, Trophic.Guild, N)), 
                      by = c("barcode", "Trophic.Guild"))
dt.final <- dt.final %>% replace_na(list(N = 0))
dt.final <- dt.final %>% filter(!stratum_name == "Impounded--Offshore") %>% droplevels()


## investigate the new dataset
table(dt.final$snag, dt.final$Trophic.Guild)
table(dt.final$snag, dt.final$N)
table(dt.final$N, dt.final$Trophic.Guild)
sum(is.na(dt.final))
dim(dt.final)
str(dt.final)

####################################################
## plot dataframe prep

## number of stratum points
strat_points <- as.data.frame(dt.final %>% 
                            group_by(stratum_name) %>% 
                            summarize(totpoints = n()))
colnames(strat_points) <- c("stratum_name", "strat_p")

## number of snag points
snag_points <- as.data.frame(dt.final %>% 
                               group_by(snag) %>% 
                               summarize(totpoints = n()))
colnames(snag_points) <- c("snag", "snag_p")

## add columns to the dt
dt.prop <- left_join(dt.final, strat_points, by = "stratum_name")
dt.prop <- left_join(dt.prop, snag_points, by = "snag")

cbPalette <- c("#4F4F4F", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


####################################################
## plots

dev.off()
## plot a hist of the num of spec per guild per site
p_bw <- ggplot(dt.prop, aes(x = N)) + geom_histogram()+
  ylab("Number of Sites") +
  xlab("Number of Species per Guild") +
  scale_x_continuous(breaks = 0:10)
p_bw


## plot a hist of the num of spec per guild per site colored by trophic guild
p_col <- ggplot(dt.prop, aes(x = N, fill = Trophic.Guild)) + geom_histogram()+
  ylab("Number of Sampling Sites") +
  xlab("Number of Species per Food Chain Level") +
  scale_x_continuous(breaks = 0:10) +
  scale_fill_manual(values = cbPalette, name = element_text("Food Chain Level"), 
                    labels = c("Herbivore", "Omnivore", "General Invertivore",
                                            "Bottom Feeding Invertivore", "Piscivore", "Planktivore")) +
  theme(text = element_text(size=15)) 
p_col
#ggsave(filename = "species_per_guild.png", plot = last_plot(), dpi = 1000)


## plot the black/white and color, hist and barplots faceted by SNAG
ggplot(dt.prop, aes(x = N)) +
  geom_bar(aes(y=..prop.., group = 1, fill = Trophic.Guild)) +
  scale_y_continuous(labels=percent_format()) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(breaks = 0:10)+
  facet_grid( ~ snag)


p_col + facet_grid( ~ snag)


## plot the color hist faceted by Trophic Guild
p_col + facet_grid( ~ Trophic.Guild)
#ggsave(filename = "species_per_guild_facet_guild.png", plot = last_plot(), dpi = 1000)


## plot the color hist faceted by snag and Trophic Guild
p_col + facet_grid(snag ~ Trophic.Guild)


## plot the color hist faceted by snag and stratum name
p_col + facet_grid(snag ~ stratum_name)


## plot the color hist faceted by Trophic Guild and stratum name
p_col + facet_grid(Trophic.Guild ~ stratum_name)


with(dt.final, tapply(N, Trophic.Guild, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


####################################################
## MODELS

## model just by snag
modelOut0 <- glm( N ~ snag , family = 'poisson', data = dt.final)
summary(modelOut0)
## shows the standardized means and confidence intervals
tidy(modelOut0, exponentiate = TRUE, conf.int = TRUE)


## model snag and trophic guild
modelOut1.1 <- glm( N ~ snag + Trophic.Guild, family = 'poisson', data = dt.final)
summary(modelOut1.1)
## shows the standardized means and confidence intervals
tidy(modelOut1.1, exponentiate = TRUE, conf.int = TRUE)


## model snag and trophic guild with an interaction term
modelOut1.2 <- glm( N ~ snag * Trophic.Guild, family = 'poisson', data = dt.final)
summary(modelOut1.2)
## shows the standardized means and confidence intervals
tidy(modelOut1.2, exponentiate = TRUE, conf.int = TRUE)
## shows there is an interaction and that it is significant
anova(modelOut1.2, test = "Chisq")


## model snag and habitat type
modelOut2.1 <- glm( N ~ snag + stratum_name, family = 'poisson', data = dt.final)
summary(modelOut2.1)
## shows the standardized means and confidence intervals
tidy(modelOut2.1, exponentiate = TRUE, conf.int = TRUE)


## model snag and habitat type with an interaction term
modelOut2.2 <- glm( N ~ snag * stratum_name, family = 'poisson', data = dt.final)
summary(modelOut2.2)
## shows the standardized means and confidence intervals
tidy(modelOut2.2, exponentiate = TRUE, conf.int = TRUE)
## shows there is an interaction and that it is significant
anova(modelOut2.2, test = "Chisq")


## model snag and habitat type
modelOut3.1 <- glm( N ~ snag + Trophic.Guild + stratum_name, family = 'poisson', data = dt.final)
summary(modelOut3.1)
## shows the standardized means and confidence intervals
tidy(modelOut3.1, exponentiate = TRUE, conf.int = TRUE)


## model snag and habitat type with an interaction term
modelOut3.2 <- glm( N ~ Trophic.Guild * (snag + stratum_name) , family = 'poisson', data = dt.final)
summary(modelOut3.2)
## shows the standardized means and confidence intervals
tidy(modelOut3.2, exponentiate = TRUE, conf.int = TRUE)
## shows there is an interaction and that it is significant
anova(modelOut3.2, test = "Chisq")


## make barcode a factor
#dt.final$barcode <- factor(dt.final$barcode)
## model barcode and trophic guild
#modelOut4 <- glm( N ~ barcode + Trophic.Guild, family = 'poisson', data = dt.final)
#summary(modelOut4)
## shows the standardized means and confidence intervals
#tidy(modelOut4, exponentiate = TRUE, conf.int = TRUE)
## shows there is an interaction and that it is significant
#anova(modelOut4, test = "Chisq")


####################################################
## plot model0 snag vs fitted values
scatterplot(dt.final$snag, modelOut0$fitted.values)


## plot model0 Trophic Guild vs fitted values
scatterplot(dt.final$Trophic.Guild, modelOut0$fitted.values)


## plot model1.1 snag vs fitted values
scatterplot(dt.final$snag, modelOut1.1$fitted.values)


## plot model1.1 Trophic Guild vs fitted values
scatterplot(dt.final$Trophic.Guild, modelOut1.1$fitted.values)


