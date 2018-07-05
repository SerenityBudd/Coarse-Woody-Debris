source("libraries.R")

# load in the data
load("data/funcdiv3.Rda")
head(funcdiv3)
str(funcdiv3)

##################################################
# chi square test for R.Guild1
table(funcdiv3$R.Guild1, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$R.Guild1)

# remove the "" category 
R.Guild1.rm <- funcdiv3 %>% 
  filter(!R.Guild1 == "", !R.Guild1 == "Catadromous") %>%
  droplevels()

summary(R.Guild1.rm$R.Guild1)

# create a contingency table
R.Guild1.tbl <- table(R.Guild1.rm$R.Guild1, R.Guild1.rm$snag)
R.Guild1.tbl

# run a chi square model
test.R.G.1 <- chisq.test(R.Guild1.tbl)
test.R.G.1
summary(R.Guild1.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.R.G.1$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(R.Guild1.rm$R.Guild1)

##################################################
# chi square test for R.Guild2
table(funcdiv3$R.Guild2, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$R.Guild2)

# remove the "" category 
R.Guild2.rm <- funcdiv3 %>% 
  filter(!R.Guild2 == "") %>%
  droplevels()

summary(R.Guild2.rm$R.Guild2)

# create a contingency table
R.Guild2.tbl <- table(R.Guild2.rm$R.Guild2, R.Guild2.rm$snag)
R.Guild2.tbl

# run a chi square model
test.R.G.2 <- chisq.test(R.Guild2.tbl)
test.R.G.2
summary(R.Guild2.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.R.G.2$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(R.Guild2.rm$R.Guild2)


##################################################
# chi square test for R.Guild3
table(funcdiv3$R.Guild3, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$R.Guild3)

# remove the "" category 
R.Guild3.rm <- funcdiv3 %>% 
  filter(!R.Guild3 == "") %>%
  droplevels()

summary(R.Guild3.rm$R.Guild3)

# create a contingency table
R.Guild3.tbl <- table(R.Guild3.rm$R.Guild3, R.Guild3.rm$snag)
R.Guild3.tbl

# run a chi square model
test.R.G.3 <- chisq.test(R.Guild3.tbl)
test.R.G.3
summary(R.Guild3.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.R.G.3$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(R.Guild3.rm$R.Guild3)


##################################################
# chi square test for Trophic Guild
table(funcdiv3$Trophic.Guild, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$Trophic.Guild)

# create a contingency table
Trophic.Guild.Tbl <- table(funcdiv3$Trophic.Guild, funcdiv3$snag)
Trophic.Guild.Tbl

# run a chi square model
test.T.G <- chisq.test(Trophic.Guild.Tbl)
test.T.G
summary(Trophic.Guild.Tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.T.G$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association


##################################################
# chi square test for F.Guild1
table(funcdiv3$F.Guild1, funcdiv3$snag)

funcdiv3[funcdiv3$F.Guild1 == "No feed", "Fishcode"]
funcdiv3[funcdiv3$Fishcode == "ABLP","Fishcode"]

# see what the levels are called
levels(funcdiv3$F.Guild1)

# remove the "" category 
F.Guild1.rm <- funcdiv3 %>% filter(!F.Guild1 == "" & !F.Guild1 == "No feed") %>% droplevels()
summary(F.Guild1.rm$F.Guild1)

# create a contingency table
F.Guild1.tbl <- table(F.Guild1.rm$F.Guild1, F.Guild1.rm$snag)
F.Guild1.tbl

# run a chi square model
test.F.G.1 <- chisq.test(F.Guild1.tbl)
test.F.G.1
summary(F.Guild1.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.F.G.1$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(F.Guild1.rm$F.Guild1)


##################################################
# chi square test for F.Guild2
table(funcdiv3$F.Guild2, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$F.Guild2)

# remove the "" category 
F.Guild2.rm <- funcdiv3 %>% filter(!F.Guild2 == "" & !F.Guild2 == "No feed") %>% droplevels()
summary(F.Guild2.rm$F.Guild2)

# create a contingency table
F.Guild2.tbl <- table(F.Guild2.rm$F.Guild2, F.Guild2.rm$snag)
F.Guild2.tbl

# run a chi square model
test.F.G.2 <- chisq.test(F.Guild2.tbl)
test.F.G.2
summary(F.Guild1.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.F.G.2$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(F.Guild2.rm$F.Guild2)


##################################################
# chi square test for F.Guild3
table(funcdiv3$F.Guild3, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$F.Guild3)

# remove the "" category 
summary(funcdiv3$F.Guild3)

funcdiv3[funcdiv3$F.Guild3 == "Grazer","Fishcode"]
funcdiv3[funcdiv3$F.Guild3 == "Hunter","Fishcode"]
funcdiv3[funcdiv3$F.Guild3 == "Lie-in-wait/ambush","Fishcode"]

funcdiv3[funcdiv3$Fishcode == "LSSR","Common.Name"]
funcdiv3[funcdiv3$Fishcode == "SNSG","Common.Name"]
funcdiv3[funcdiv3$Fishcode == "SKCB","Common.Name"]
funcdiv3[funcdiv3$Fishcode == "STCT","Common.Name"]

F.Guild3.rm <- funcdiv3 %>% 
  filter(!F.Guild3 == "" & !F.Guild3 == "No feed" & !F.Guild3 == "Grazer" & 
           !F.Guild3 == "Hunter" & !F.Guild3 == "Lie-in-wait/ambush") %>%
  droplevels()

# create a contingency table
F.Guild3.tbl <- table(F.Guild3.rm$F.Guild3, F.Guild3.rm$snag)
F.Guild3.tbl

# run a chi square model
test.F.G.3 <- chisq.test(F.Guild3.tbl)
test.F.G.3
summary(Trophic.Guild.Tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.F.G.3$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association
levels(F.Guild3.rm$F.Guild3)


##################################################
# chi square test for Parental Care
table(funcdiv3$Parental.Care, funcdiv3$snag)

# see what the levels are called
levels(funcdiv3$Parental.Care)

# create a contingency table
Parental.Care.tbl <- table(funcdiv3$Parental.Care, funcdiv3$snag)
Parental.Care.tbl

# run a chi square model
test.P.C <- chisq.test(Parental.Care.tbl)
test.P.C
summary(Parental.Care.tbl)
# the test says there is an association, they are not independent

# look at a plot of the residuals
par(mfrow = c(1,1))
corrplot(test.P.C$residuals, is.cor = FALSE)
# the residuals show the specific trend, help us interpret the association