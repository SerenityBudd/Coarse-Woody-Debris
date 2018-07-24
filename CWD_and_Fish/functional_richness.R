source("libraries.R")
load("data/funcdiv8.Rda")
load("data/funcdiv4.8.13.new.Rda")

funcdiv8 <- funcdiv4.8.13.new %>%
  filter(pool == "08")
#######################################################
## DATA CLEANING -- Trophic.Guilds

## create a dataframe specifying the num of fish per trophic guild per barcode
xxx.fd1 <- funcdiv8 %>% group_by(barcode) %>% count(Trophic.Guild)
colnames(xxx.fd1)[3] <- "Num_Fish_per_Trophic_Guild"
xxx.fd1 <- xxx.fd1[complete.cases(xxx.fd1),]


## create a dataframe specifying the number of unique trophic guilds per barcode
yyy.fd1 <- xxx.fd1 %>% group_by(barcode) %>% count(barcode)
colnames(yyy.fd1)[2] <- "Num_Guilds"


#######################################################
## T TESTS - Trophic Guilds

## create a richness dataframe to do a t test on, combinging snag and yyy.fd
fd.richness.t1 <- left_join(yyy.fd1, select(funcdiv8, c(barcode, snag)), by = "barcode") %>% distinct


## there is a signigficant diff in richness between sites with and without CWD
with(fd.richness.t1, t.test(Num_Guilds~snag, alternative = "less")) 


#######################################################
## DATA CLEANING - R.Guild1

## create a dataframe specifying the number of fish per RGuild per barcode
xxx.fd2 <- funcdiv8 %>% group_by(barcode) %>% count(Spawning.Group)
colnames(xxx.fd2)[3] <- "Num_Fish_per_Spawning.Group"
#xxx.fd2 <- xxx.fd2 %>% filter(!R.Guild1 == ""
 #                       & !R.Guild1 == "Catadromous"
  #                      & !R.Guild1 == "Bearer") %>% droplevels()


## create a dataframe specifying the number of unique RGuilds per barcode
yyy.fd2 <- xxx.fd2 %>% group_by(barcode) %>% count(barcode)
colnames(yyy.fd2)[2] <- "Num_Spawning.Group"


#######################################################
## T TESTS

## create a richness dataframe to do a t test on, combinging snag and yyy
fd.richness.t2 <- left_join(yyy.fd2, select(funcdiv8, c(barcode, snag)), by = "barcode") %>% distinct


## there is a not signigficant diff in richness between sites with and without CWD
with(fd.richness.t2, t.test(Num_Spawning.Group~snag, alternative = "less")) 



#######################################################
## DATA CLEANING - my guilds

dtdt <- funcdiv8 %>% 
  select(c(barcode, snag, Trophic.Guild, Spawning.Group)) %>%
  #filter(!R.Guild1 == ""
    #     & !R.Guild1 == "Catadromous"
     #    & !R.Guild1 == "Bearer") %>% droplevels() %>%
  filter(!is.na(Trophic.Guild) & !is.na(snag) & !is.na(Spawning.Group)) %>%
  mutate(Guild = factor(paste(Trophic.Guild, Spawning.Group)))

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

