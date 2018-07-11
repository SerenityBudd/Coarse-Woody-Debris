source("libraries.R")
load("data/funcdiv3.Rda")

head(funcdiv3)

## make common name a character
funcdiv3$Common.Name <- as.character(funcdiv3$Common.Name)


tbl <- funcdiv3 %>%
  ## only sites in unstructured main channel and backwater
  filter(stratum_name %in% "Backwater, Contiguous Shoreline"
         | stratum_name %in% "Main Channel Border--Unstructured") %>%
  ## only in sites where they electroshocked for fish during the day
  filter(gear %in% "D") %>%
  ## select the relevant columns
  select(c(Common.Name, barcode, snag, 
           stratum_name, effmin, Trophic.Guild)) %>%
  droplevels()
## shorten the name to make easier to read
levels(tbl$stratum_name) <- c("Backwater", "Main Channel Border")
## remove NAs
tbl <- tbl[complete.cases(tbl),]


sum(is.na(tbl))
summary(tbl)


###################################################

tbl2 <- tbl %>% 
  group_by(Common.Name) %>%
  ## add a column of total of each species, proportion collected with CWD presense
  summarise("N" = n(), "propYes" = round(sum(snag == "Yes")/(sum(snag == "Yes") + sum(snag == "No")),3)) %>%
  ## add a column of proportion collected CWD 
  mutate("propNo" = 1 - propYes) %>% 
  ## make it a dataframe
  as.data.frame(.) 

## create a table of fish and CWD
ttt <- table(tbl$Common.Name, tbl$snag)


## chisq.test(x=c(No CWD, Yes CWD),p=c(.5,.5)), goodness of fit
## assume the fish are equally likely to be at site regardless of CWD presence
for (i in 1:56) {
  ## do the chi squared test on each species
  y <- chisq.test(x=c(ttt[i,1], ttt[i,2]),p=c(.5,.5))
  ## make a column for the p value
  tbl2$p.value[i] <- round(y$p.value,4)
  ## make a table of logicals about significance 
  tbl2$significant[i] <- tbl2$p.value[i] < .05
  ## make a table of the results, which sites the fish prefer 
  tbl2$preference[i] <- ifelse(tbl2$significant[i] == FALSE, "none", 
                               ifelse(ttt[i,1] < ttt[i,2], "CWD", "no CWD"))
}

## table of output from Chi-squared test for each fish species
tbl2


## modify the table for 'xtable'
tbl3 <- tbl2[,-c(1,5)]
rownames(tbl3) <- tbl2$Common.Name


## print out the table in LaTeX format 
xtable(tbl3)


###################################################

tbl4 <- tbl %>% 
  group_by(stratum_name, Common.Name) %>%
  ## add a column of total of each species, proportion collected with CWD presense
  summarise("N" = n(), "propYes" = round(sum(snag == "Yes")/(sum(snag == "Yes") + sum(snag == "No")),3)) %>%
  ## add a column of proportion collected CWD 
  mutate("propNo" = 1 - propYes) %>% 
  ## make it a dataframe
  as.data.frame(.) 


## create a table of fish and CWD for the BACKWATER
tbl.back <- tbl %>% filter(stratum_name %in% "Backwater") 
ttt.backwater <- table(tbl.back$Common.Name, tbl.back$snag)


## chisq.test(x=c(No CWD, Yes CWD),p=c(.5,.5)), goodness of fit
## assume the fish are equally likely to be at site regardless of CWD presence
for (i in 1:49) {
  ## do the chi squared test on each species
  y <- chisq.test(x=c(ttt.backwater[i,1], ttt.backwater[i,2]),p=c(.5,.5))
  ## make a column for the p value
  tbl4$p.value[i] <- round(y$p.value,4)
  ## make a table of logicals about significance 
  tbl4$significant[i] <- tbl4$p.value[i] < .05
  ## make a table of the results, which sites the fish prefer 
  tbl4$preference[i] <- ifelse(tbl4$significant[i] == FALSE, "none", 
                               ifelse(ttt.backwater[i,1] < ttt.backwater[i,2], "CWD", "no CWD"))
}


## create a table of fish and CWD for the MAIN CHANNEL
tbl.main <- tbl %>% filter(stratum_name %in% "Main Channel Border")
ttt.mainchannel <- table(tbl.main$Common.Name, tbl.main$snag)


## chisq.test(x=c(No CWD, Yes CWD),p=c(.5,.5)), goodness of fit
## assume the fish are equally likely to be at site regardless of CWD presence
for (i in 50:98) {
  ## do the chi squared test on each species
  y <- chisq.test(x=c(ttt.mainchannel[i-49,1], ttt.mainchannel[i-49,2]),p=c(.5,.5))
  ## make a column for the p value
  tbl4$p.value[i] <- round(y$p.value,4)
  ## make a table of logicals about significance 
  tbl4$significant[i] <- tbl4$p.value[i] < .05
  ## make a table of the results, which sites the fish prefer 
  tbl4$preference[i] <- ifelse(tbl4$significant[i] == FALSE, "none", 
                               ifelse(ttt.mainchannel[i-49,1] < ttt.mainchannel[i-49,2], "CWD", "no CWD"))
}


## table of output from Chi-squared test for each fish species
tbl4


###################################################

xxx.tbl <- tbl %>% group_by(stratum_name, snag) %>% count(Common.Name)
colnames(xxx.tbl)[4] <- "Num_Fish_per_Species"


## spread the dataframe so each col is a fishcode
aaa.tbl <- spread(data = xxx.tbl, value = Num_Fish_per_Species, Common.Name) 
## replace the NAs with zero
aaa.tbl[is.na(aaa.tbl)] <- 0
## make the tibble a dataframe
aaa.tbl <- data.frame(aaa.tbl)


## make a dataframe to do analysis on, leave stratum and snag out
bbb.tbl <- select(aaa.tbl, -c(stratum_name, snag))


## uses the bray dissimilarity metric
vegdist(x = bbb.tbl, method = "bray")


## create a df with the dissimilarity matrix
bray.dis <- data.frame("Backwater No" = c(0.2484745, 0.5453902, 0.4957565),
                       "Backwater Yes" = c(NA, 0.6702122, 0.4384222),
                       "Main Channel No" = c(NA, NA, 0.4008342))
rownames(bray.dis) <- c("Backwater.Yes", "Main.Channel.No", "Main.Channel.Yes")

## creat a df with the similarity matrix
bray.sim <- 1 - bray.dis

#         stratum_name   snag 
# 1           Backwater   No  
# 2           Backwater  Yes  
# 3 Main Channel Border   No  
# 4 Main Channel Border  Yes 


###################################################

## create a df for (catch per unit effort) per barcode
CPE.tbl <- tbl %>% group_by(barcode) %>% summarize("CPE" = n()/first(effmin))


## create a df to plot CPE, summarize over stratum & snag, using mean and sd
CPE.plot <- left_join(tbl, CPE.tbl, by = "barcode") %>% 
  group_by(stratum_name, snag) %>%
  summarise("Mean" = mean(CPE), "SD" = sd(CPE)) %>%
  data.frame(.)


## ggplot a bar chart of the "CPE by Habitat type and Snag"
ggplot(CPE.plot, aes(x=stratum_name, y=Mean, fill=snag)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
 # geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
     #           position=position_dodge(.9)) +
  labs(title="CPE by Habitat type and Snag", x="Habitat Type", y = "CPE")+
  theme_classic() +
  scale_fill_manual(values=c('steelblue','#E69F00'))


## create a df to run kruskal.test on 
CPE.test <- left_join(tbl, CPE.tbl, by = "barcode") 
## Kruskal-Wallis Test, we can decide whether the population distributions are identical without assuming them to follow the normal distribution


kruskal.test(CPE ~ stratum_name, data = CPE.test)
kruskal.test(CPE ~ snag, data = CPE.test)
## the populations are nonidentical 


###################################################

## create a df for (Abundance) per barcode
n.tbl <- tbl %>% group_by(barcode) %>% summarize("Abundance" = n())


## create a df to plot Abundance, summarize over stratum & snag, using mean and sd
n.plot <- left_join(tbl, n.tbl, by = "barcode") %>% 
  group_by(stratum_name, snag) %>%
  summarise("Mean" = mean(Abundance), "SD" = sd(Abundance)) %>%
  data.frame(.)


## ggplot a bar chart of the "Abundance by Habitat type and Snag"
ggplot(n.plot, aes(x=stratum_name, y=Mean, fill=snag)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  # geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,
  #           position=position_dodge(.9)) +
  labs(title="Abundance by Habitat type and Snag", x="Habitat Type", y = "Number of Fish")+
  theme_classic() +
  scale_fill_manual(values=c('steelblue','#E69F00'))


## create a df to run kruskal.test on 
n.test <- left_join(tbl, n.tbl, by = "barcode")


kruskal.test(Abundance ~ stratum_name, data = n.test)
kruskal.test(Abundance ~ snag, data = n.test)
## the populations are nonidentical 


