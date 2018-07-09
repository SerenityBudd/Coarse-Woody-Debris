source("libraries.R")
load("data/funcdiv3.Rda")
library(mlogit)

head(funcdiv3)
str(funcdiv3)

#funcdiv4 <- funcdiv3 %>% droplevels()

#funcdiv4$Trophic.Guild2 <- relevel(funcdiv4$Trophic.Guild, ref = "1")
#test <- multinom(Trophic.Guild2 ~ snag, data = funcdiv4,
 #                na.action =  na.exclude)
#summary(test)

#z <- summary(test)$coefficients/summary(test)$standard.errors
#p <- (1 - pnorm(abs(z), 0, 1)) * 2
#p


funcdiv4 <- funcdiv3 %>% droplevels()
head(funcdiv4)
str(funcdiv4)

R.Guild1.rm <- funcdiv3 %>% 
  filter(!R.Guild1 == "", !R.Guild1 == "Catadromous") %>%
  droplevels() %>%
  select(c(R.Guild1, snag, stratum_name, barcode))

head(R.Guild1.rm)
str(R.Guild1.rm)

#### using mlogit
R.Guild1.ml <- mlogit.data(R.Guild1.rm, choice="R.Guild1", shape="wide")
head(R.Guild1.ml)
str(R.Guild1.ml)

ml.R.Guild1.1 <- mlogit(R.Guild1 ~  snag + stratum_name, R.Guild1.ml)

summary(ml.R.Guild1.1)



#### using multinom
ml1 <- multinom(R.Guild1 ~  snag + stratum_name, R.Guild1.rm)
ml1
summary(ml1)
z <- summary(ml1)$coefficients/summary(ml1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
