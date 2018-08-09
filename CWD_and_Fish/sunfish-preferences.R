source("libraries.R")
load("data/funcdiv4.8.13.Rda")


fam.funcdiv <- filter(funcdiv4.8.13, Family.Name %in% c("Centrarchidae")) %>%
  droplevels()


## make common name a character
fam.funcdiv$Common.Name <- as.character(fam.funcdiv$Common.Name)

## make a dataframe of the environmental variables and trophic guild
tbl0 <- fam.funcdiv %>%
  ## select the relevant columns
  select(c(Common.Name, barcode, snag, 
           stratum_name, sdate, pool, Trophic.Guild
           #, depth, temp, current
  )) %>%
  droplevels()
## remove NAs
tbl0 <- tbl0[complete.cases(tbl0),]

sum(is.na(tbl0))
summary(tbl0)

## number of species per barcode, does not include zeros, barcode only listed for a fish if it was found there
datty <-
  tbl0 %>% group_by(barcode, Common.Name) %>% summarise(n = n())

## number of species per barcode, includes zeros, each barcode is listed 8 times, one for each fish
datty2 <- 
  ddply(datty, .(barcode, Common.Name), summarise, N = sum(n), .drop=FALSE)

## number of species per barcode, includes zeros, includes data collected at the sampling point, stratum, snag, effmin, pool
datty3 <- left_join(datty2, select(tbl0, -c(Common.Name, Trophic.Guild)), by = c("barcode")) %>% distinct()
#save(datty3, file = "data/datty3.Rda")

## add a column for stratum and snag, easier to make tables with
datty3$ss <- paste(datty3$stratum_name, datty3$snag)

## datatable to make into abundance table by habitat (stratum + snag)
datty4 <- 
  ddply(datty3, .(ss, Common.Name), summarise, Nn = sum(N), .drop=FALSE)

## table of abundances by species (as rows)
tabbs <- spread(data = datty4, key = ss, value = Nn)
xtable(tabbs)

## total number of fish at each habitat (stratum + snag)
colSums(tabbs[,-1])

## number of sampling sites per habitat stratum & snag
rowSums(with(filter(datty3, Common.Name == "Black crappie"), table(ss, barcode)))

dim(datty3)
# 38576
length(unique(tbl0$barcode)) * 8

##############################################################################
## make a species specific dataframe ---- Black crappie
BKCPdat <- datty3 %>% 
  filter(Common.Name == "Black crappie")

#############################################
## poisson regression 

BKCP.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = BKCPdat)
summary(BKCP.m1)
anova(BKCP.m1, test = "Chisq")
BKCP.m1coeff <- exp(BKCP.m1$coefficients)

#############################################
## without interaction 

BKCP.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = BKCPdat)
summary(BKCP.m2)
anova(BKCP.m2, test = "Chisq")
BKCP.m2coeff <- exp(BKCP.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Bluegill
BLGLdat <- datty3 %>% 
  filter(Common.Name == "Bluegill") 

#############################################
## poisson regression 

BLGL.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = BLGLdat)
summary(BLGL.m1)
anova(BLGL.m1, test = "Chisq")
BLGL.m1coeff <- exp(BLGL.m1$coefficients)

#############################################
## without interaction 

BLGL.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = BLGLdat)
summary(BLGL.m2)
anova(BLGL.m2, test = "Chisq")
BLGL.m2coeff <- exp(BLGL.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Green sunfish
GNSFdat <- datty3 %>% 
  filter(Common.Name == "Green sunfish") 

#############################################
## poisson regression 

GNSF.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = GNSFdat)
summary(GNSF.m1)
anova(GNSF.m1, test = "Chisq")
GNSF.m1coeff <- exp(GNSF.m1$coefficients)

#############################################
## without interaction 

GNSF.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = GNSFdat)
summary(GNSF.m2)
anova(GNSF.m2, test = "Chisq")
GNSF.m2coeff <- exp(GNSF.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Largemouth bass
LMBSdat <- datty3 %>% 
  filter(Common.Name == "Largemouth bass") 

#############################################
## poisson regression 

LMBS.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = LMBSdat)
summary(LMBS.m1)
anova(LMBS.m1, test = "Chisq")
LMBS.m1coeff <- exp(LMBS.m1$coefficients)

#############################################
## without interaction 

LMBS.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = LMBSdat)
summary(LMBS.m2)
anova(LMBS.m2, test = "Chisq")
LMBS.m2coeff <- exp(LMBS.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Orangespotted sunfish
OSSFdat <- datty3 %>% 
  filter(Common.Name == "Orangespotted sunfish") 

#############################################
## poisson regression 

OSSF.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = OSSFdat)
summary(OSSF.m1)
anova(OSSF.m1, test = "Chisq")
OSSF.m1coeff <- exp(OSSF.m1$coefficients)

#############################################
## without interaction 

OSSF.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = OSSFdat)
summary(OSSF.m2)
anova(OSSF.m2, test = "Chisq")
OSSF.m2coeff <- exp(OSSF.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Rock bass
RKBSdat <- datty3 %>% 
  filter(Common.Name == "Rock bass") 

#############################################
## poisson regression 

RKBS.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = RKBSdat)
summary(RKBS.m1)
anova(RKBS.m1, test = "Chisq")
RKBS.m1coeff <- exp(RKBS.m1$coefficients)

#############################################
## without interaction 

RKBS.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = RKBSdat)
summary(RKBS.m2)
anova(RKBS.m2, test = "Chisq")
RKBS.m2coeff <- exp(RKBS.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- Smallmouth bass
SMBSdat <- datty3 %>% 
  filter(Common.Name == "Smallmouth bass") 

#############################################
## poisson regression 

SMBS.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = SMBSdat)
summary(SMBS.m1)
anova(SMBS.m1, test = "Chisq")
SMBS.m1coeff <- exp(SMBS.m1$coefficients)

#############################################
## without interaction 

SMBS.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = SMBSdat)
summary(SMBS.m2)
anova(SMBS.m2, test = "Chisq")
SMBS.m2coeff <- exp(SMBS.m2$coefficients)


##############################################################################
## make a species specific dataframe ---- White crappie
WTCPdat <- datty3 %>% 
  filter(Common.Name == "White crappie")

#############################################
## poisson regression 

WTCP.m1 <- glm(formula = N ~ stratum_name + snag, family = quasipoisson(link=log), data = WTCPdat)
summary(WTCP.m1)
anova(WTCP.m1, test = "Chisq")
WTCP.m1coeff <- exp(WTCP.m1$coefficients)

#############################################
## without interaction 

WTCP.m2 <- glm(formula = N ~ stratum_name*snag, family = quasipoisson(link=log), data = WTCPdat)
summary(WTCP.m2)
anova(WTCP.m2, test = "Chisq")
WTCP.m2coeff <- exp(WTCP.m2$coefficients)
