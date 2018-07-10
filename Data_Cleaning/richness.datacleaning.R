source("libraries.R")

# load in the data
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
                      by = c("barcode", "Trophic.Guild")) %>% 
  replace_na(list(N = 0)) %>% 
  filter(!stratum_name == "Impounded--Offshore") %>% 
  droplevels()

## investigate the new dataset
table(dt.final$snag, dt.final$Trophic.Guild)
table(dt.final$snag, dt.final$N)
table(dt.final$N, dt.final$Trophic.Guild)
sum(is.na(dt.final))
dim(dt.final)
str(dt.final)


## make the datatable wide
dt.rich <- spread(data = dt.final, value = N, Trophic.Guild)
head(dt.rich)
dim(dt.rich)
str(dt.rich)

#save(dt.rich, file = "data/dt.rich.Rda")

