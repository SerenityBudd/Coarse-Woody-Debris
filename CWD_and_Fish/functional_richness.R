source("libraries.R")
load("data/funcdiv3.Rda")


#######################################################
## DATA CLEANING -- Trophic.Guilds

## create a dataframe specifying the num of fish per trophic guild per barcode
xxx1 <- funcdiv3 %>% group_by(barcode) %>% count(Trophic.Guild)
colnames(xxx1)[3] <- "Num_Fish_per_Trophic_Guild"
xxx1 <- xxx1[complete.cases(xxx1),]


## create a dataframe specifying the number of unique trophic guilds per barcode
yyy1 <- xxx1 %>% group_by(barcode) %>% count(barcode)
colnames(yyy1)[2] <- "Num_Guilds"


#######################################################
## T TESTS - Trophic Guilds

## create a richness dataframe to do a t test on, combinging snag and yyy1
fd.richness.t1 <- left_join(yyy1, select(funcdiv3, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in richness between sites with and without CWD
with(fd.richness.t1, t.test(Num_Guilds~snag, alternative = "less")) 


#######################################################
## DATA CLEANING - R.Guild1

## create a dataframe specifying the number of fish per RGuild per barcode
xxx2 <- funcdiv3 %>% group_by(barcode) %>% count(R.Guild1)
colnames(xxx2)[3] <- "Num_Fish_per_RGuild1"
xxx2 <- xxx2 %>% filter(!R.Guild1 == ""
                        & !R.Guild1 == "Catadromous"
                        & !R.Guild1 == "Bearer") %>% droplevels()


## create a dataframe specifying the number of unique RGuilds per barcode
yyy2 <- xxx2 %>% group_by(barcode) %>% count(barcode)
colnames(yyy2)[2] <- "Num_RGuilds"


#######################################################
## T TESTS

## create a richness dataframe to do a t test on, combinging snag and yyy
fd.richness.t2 <- left_join(yyy2, select(funcdiv3, c(barcode, snag)), by = "barcode") %>% distinct


## there is a not signigficant diff in richness between sites with and without CWD
with(fd.richness.t2, t.test(Num_RGuilds~snag, alternative = "less")) 



#######################################################
## DATA CLEANING - my guilds

dtdt <- funcdiv3 %>% 
  select(c(barcode, snag, Trophic.Guild, R.Guild1)) %>%
  filter(!R.Guild1 == ""
         & !R.Guild1 == "Catadromous"
         & !R.Guild1 == "Bearer") %>% droplevels() %>%
  filter(!is.na(Trophic.Guild) & !is.na(snag)) %>%
  mutate(Guild = factor(paste(Trophic.Guild, R.Guild1)))

## create a dataframe specifying the number of fish per my_guilds per barcode
dtdt2 <- dtdt %>% group_by(barcode) %>% count(Guild)
colnames(dtdt2)[2] <- "Num_Fish_per_Guild"


## create a dataframe specifying the number of unique my_guilds per barcode
dtdt3 <- dtdt2 %>% group_by(barcode) %>% count(barcode)
colnames(dtdt3)[2] <- "Num_Guilds"


#######################################################
## T TESTS

## create a richness dataframe to do a t test on, combinging snag and yyy
dtdt.richness.t <- left_join(dtdt3, select(dtdt, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in richness between sites with and without CWD
with(dtdt.richness.t, t.test(Num_Guilds~snag, alternative = "less")) 



