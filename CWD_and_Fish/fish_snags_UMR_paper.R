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
           stratum_name, effsec, Trophic.Guild)) %>%
  droplevels()
## shorten the name to make easier to read
levels(tbl$stratum_name) <- c("Backwater", "Main Channel Border")
## remove NAs
tbl <- tbl[complete.cases(tbl),]


sum(is.na(tbl))
summary(tbl)


tbl2 <- tbl %>% 
  group_by(Common.Name) %>%
  ## add a column of proportion collected with CWD presense
  summarize("propYes" = round(sum(snag == "Yes")/(sum(snag == "Yes") + sum(snag == "No")),3)) %>% 
  ## add a column of proportion collected CWD 
  mutate("propNo" = 1 - propYes) %>% 
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
