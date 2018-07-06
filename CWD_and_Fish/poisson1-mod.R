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


dt.p <- dt.final %>% filter(Trophic.Guild == "Omnivore") %>% droplevels()
table(dt.p$snag, dt.p$Trophic.Guild)
levels(dt.p$stratum_name) <- c("Backwater area",
                               "Impounded area",
                               "Main channel border",
                               "Wing dam area",
                               "Side channel")

####################################################
## PLOTS
dev.off()
## plot a hist of the num of spec per guild per site
p_bw_o <- ggplot(dt.p, aes(x = N)) + geom_histogram()+
  ylab("Number of Sites") +
  xlab("Number of Omnivores") +
  scale_x_continuous(breaks = 0:10)
p_bw_o


## plot a hist of the num of spec per guild per site colored by trophic guild
p_col_o <- ggplot(dt.p, aes(x = N, fill = Trophic.Guild)) + geom_histogram()+
  ylab("Number of Sampling Sites") +
  xlab("Number of Omnivores") +
  scale_x_continuous(breaks = 0:10) +
  scale_fill_manual(values = "#E69F00", name = element_text("Food Chain Level"), 
                    labels = c("Omnivore")) +
  theme(text = element_text(size=20)) 
p_col_o
#ggsave(filename = "omnivores_per_site.png", plot = last_plot(), dpi = 1000)


## plot the color hist faceted by snag
ggplot(dt.p, aes(x = N)) +
  geom_bar(aes(y=..prop.., group = 1), fill = "#E69F00") +
  scale_y_continuous(labels=percent_format()) +
  scale_x_continuous(breaks = 0:10)+
  ylab("Proportion of Sampling Sites") +
  xlab("Number of Omnivores") +
  facet_grid( ~ snag) +
  theme(text = element_text(size=20)) 
#ggsave(filename = "omnivores_per_site_facet_snag.png", plot = last_plot(), dpi = 1000)


## plot the col hist faceted by stratum type
ggplot(dt.p, aes(x = N)) +
  geom_bar(aes(y=..prop.., group = 1), fill = "#E69F00") +
  scale_y_continuous(labels=percent_format()) +
  scale_x_continuous(breaks = 0:10)+
  ylab("Proportion of Sampling Sites") +
  xlab("Number of Omnivores") +
  facet_grid( ~ stratum_name) +
  theme(text = element_text(size=15)) 
#ggsave(filename = "omnivores_per_site_facet_stratum.png", plot = last_plot(), dpi = 1000)


## plot the color hist faceted by snag and stratum name
ggplot(dt.p, aes(x = N)) +
  geom_bar(aes(y=..prop.., group = 1), fill = "#E69F00") +
  scale_y_continuous(labels=percent_format()) +
  scale_x_continuous(breaks = 0:10)+
  ylab("Proportion of Sampling Sites") +
  xlab("Number of Omnivores") +
  facet_grid(snag ~ stratum_name) +
  theme(text = element_text(size=15)) 
#ggsave(filename = "omnivores_per_site_facet_snag.stratum.png", plot = last_plot(), dpi = 1000)


####################################################
## MODELS

## model just by snag
modelOut0 <- glm( N ~ snag , family = 'poisson', data = dt.p)
summary(modelOut0)
## shows the standardized means and confidence intervals
tidy(modelOut0, exponentiate = TRUE, conf.int = TRUE)


## model snag and stratum type
modelOut2.1 <- glm( N ~ snag + stratum_name, family = 'poisson', data = dt.p)
summary(modelOut2.1)
## shows the standardized means and confidence intervals
tidy(modelOut2.1, exponentiate = TRUE, conf.int = TRUE)
anova(modelOut2.1, test = "Chisq")


## model snag and stratum type with an interaction term
modelOut2.2 <- glm( N ~ snag * stratum_name, family = 'poisson', data = dt.p)
summary(modelOut2.2)
## shows the standardized means and confidence intervals
tidy(modelOut2.2, exponentiate = TRUE, conf.int = TRUE)
## shows there is an interaction and that it is significant
anova(modelOut2.2, test = "Chisq")


####################################################
## plot model0 snag vs fitted values
scatterplot(dt.p$snag, modelOut0$fitted.values)


## plot model0 Trophic Guild vs fitted values
scatterplot(dt.p$stratum_name, modelOut0$fitted.values)


## plot model1.1 snag vs fitted values
scatterplot(dt.p$snag, modelOut2.1$fitted.values)


## plot model1.1 Trophic Guild vs fitted values
scatterplot(dt.p$stratum_name, modelOut2.1$fitted.values)


