source("libraries.R")
load("data/funcdiv4.8.13.Rda")
load("data/fishinfo2.Rda")

###################################################
## Centrarchidae, 185419

fam.funcdiv <- filter(funcdiv4.8.13, 
                      Family.Name %in% c("Centrarchidae")
                      ## these are the strata with enough sampling points
                      & stratum_name %in% c("Backwater, Contiguous Shoreline", "Impounded, Shoreline", "Main Channel Border, Unstructured", "Main Channel Border, Wing Dam Area", "Side Channel Border")
                      ## when comparing MCB-U to IMP-S
                    #  & !pool == "04"
                      ) %>%
  #Impounded, Shoreline
  droplevels()


## make common name a character
fam.funcdiv$Common.Name <- as.character(fam.funcdiv$Common.Name)

## make a dataframe of the environmental variables and trophic guild
tbl0 <- fam.funcdiv %>%
  ## select the relevant columns
  select(c(Common.Name, barcode, snag, 
           stratum_name, effmin, pool, Trophic.Guild)) %>%
  droplevels()
## remove NAs
tbl0 <- tbl0[complete.cases(tbl0),]


sum(is.na(tbl0))
summary(tbl0)

###################################################
## create a community data matrix

## make a tibble of species abundance per pool x stratum combo
xxx.tbl <- tbl0 %>% group_by(pool,stratum_name) %>% count(Common.Name)
colnames(xxx.tbl)[4] <- "Num_Fish_per_Species"


## spread the dataframe so each col is a Common.Name
aaa.tbl <- spread(data = xxx.tbl, value = Num_Fish_per_Species, Common.Name) 
## replace the NAs with zero
aaa.tbl[is.na(aaa.tbl)] <- 0
## make the tibble a dataframe
aaa.tbl <- data.frame(aaa.tbl)


###################################################
## "Backwater, Contiguous Shoreline"
## "Impounded, Shoreline" --- pools 8 & 13 only
## "Main Channel Border, Unstructured" --- only use 8 & 13 when comparing to IMP-S
## "Main Channel Border, Wing Dam Area"
## "Side Channel Border"


## within the community of a specific strata, what proportion of each fish species were at sites with snag (and without snag)?
tbl2 <- tbl0 %>%
  ## filter by the stratum we are working with
  filter(stratum_name == "Main Channel Border, Unstructured") %>%
  ## group by fish species
  group_by(Common.Name) %>%
  ## add a column of total of each species, proportion collected with snag presense
  summarise("N" = n(), "propYes" = round(sum(snag == "Yes")/(sum(snag == "Yes") + sum(snag == "No")),3)) %>%
  ## add a column of proportion collected snag 
  mutate("propNo" = 1 - propYes) %>% 
  ## make it a dataframe
  as.data.frame(.) 

## create a table of fish and snag
ttt <- with(filter(tbl, stratum_name == "Main Channel Border, Unstructured"), table(Common.Name, snag))

#with(filter(tbl, stratum_name == "Stratum"), table(snag))

## chisq.test(x=c(No snag, Yes snag),p=c(.k,.j)), goodness of fit
## for strata == "Backwater, Contiguous Shoreline", p = c(.30,.70)
## for strata == "Impounded, Shoreline", p = c(.46,.54)
## for strata == "Main Channel Border, Unstructured", p = c(.31,.69) -- all pools
## for strata == "Main Channel Border, Unstructured", p = c(.37,.63) -- pools 8 & 13
## for strata == "Main Channel Border, Wing Dam Area", p = c(.87,.13)
## for strata == "Side Channel Border", p = c(.23,.77)

for (i in 1:dim(ttt)[1]) {
  ## do the chi squared test on each species
  y <- chisq.test(x=c(ttt[i,1], ttt[i,2]),p=c(.37,.63))
  ## make a column for the p value
  tbl2$p.value[i] <- round(y$p.value,4)
  ## make a table of logicals about significance 
  tbl2$significant[i] <- tbl2$p.value[i] < .05
  ## make a table of the results, which sites the fish prefer 
  tbl2$preference[i] <- ifelse(tbl2$significant[i] == FALSE, "none", 
                               ifelse(ttt[i,1] < ttt[i,2], "snag", "no snag"))
}

fishinfo2$Common.Name <- as.character(fishinfo2$Common.Name)
## table of output from Chi-squared test for each fish species, add trophic guild
tbl2 <- right_join(select(fishinfo2, c(Common.Name, Trophic.Guild)),tbl2,  by = "Common.Name")
## sort the table by trophic guild
tbl2 <- tbl2[order(tbl2$Trophic.Guild),] 

## modify the table for 'xtable'
tbl3 <- tbl2[,-c(1)]
rownames(tbl3) <- tbl2$Common.Name
tbl3

## print out the table in LaTeX format 
xtable(tbl3)

###################################################

