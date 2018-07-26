library(lme4)
source("libraries.R")
load("data/all_reduced_15.Rda")
all_reduced <- all_reduced_15 %>% dplyr::select(-c(barcode, Area, pct_prm_lotic, pct_prm_lentic, num_lotic_outl, num_lentic_outl, econ, pct_terr_shore_wetf, sinuosity, NEAR_TERR_CLASS_15_N.p, NEAR_FOREST_CLASS_15_N.p, year.p, stageht.p, stratum_name, snagyn, pct_area_le100, depth.p, current.p, substrt.p, trib.p, pct_aqveg, AQUA_CODE)) %>% na.omit()
all <- all_reduced
colnames(all) <- c("stratum", "snag", "uniq_id", "pool", "perimeter", "max_depth", "avg_depth", "total_volume", "shoreline_density_index", "pct_terrestrial_shore", "pct_perimeter_wetforest", "dist_to_land", "nearest_land_class", "dist_to_forest", "nearest_forest_class", "wingdam", "revetment")
all$snag <- factor(as.character(all$snag))

allscaled <- all

# scale numeric variables to avoid problems with convergence
allscaled$perimeter <- scale(allscaled$perimeter)
allscaled$max_depth <- scale(allscaled$max_depth)
allscaled$avg_depth <- scale(allscaled$avg_depth)
allscaled$total_volume <- scale(allscaled$total_volume)
allscaled$shoreline_density_index <- scale(allscaled$shoreline_density_index)
allscaled$pct_terrestrial_shore <- scale(allscaled$pct_terrestrial_shore)
allscaled$pct_perimeter_wetforest <- scale(allscaled$pct_perimeter_wetforest)
allscaled$dist_to_land <- scale(allscaled$dist_to_land)
allscaled$dist_to_forest <- scale(allscaled$dist_to_forest)

#=============================
# POOL 4
#=============================
pool4_scaled <- allscaled[allscaled$pool == 4,] %>% dplyr::select(-pool)

# start with the first three variables, stratum, nearest_land_class, and wingdam
m1 <- glmer(snag ~ stratum + nearest_land_class + wingdam + (1 | uniq_id),
      data = pool4_scaled,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl=list(maxfun=2e5))) 
# the model gives a warning that it won't converge. Try different optimizers, according to https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html:
afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/inst/utils/allFit.R"
eval(parse(text = getURL(afurl)))
aa <- allFit(m1)
is.OK <- sapply(aa, is, "merMod")
aa.OK <- aa[is.OK]
lapply(aa.OK, function(x) x@optinfo$conv$lme4$messages)

# these don't seem to help. According to various SO posts, probably have too few observations in each category. This may be because we have two categorical variables with a lot of categories.
# View summary anyway
summary(m1)
# maybe we could lump the land classes into something smaller: go back and use the 7-type or 15-type land classes. 

#Let's try getting rid of nearest_land_class:
m2 <- glmer(snag ~ stratum + wingdam + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m2)
anova(m2, test = "Chisq") #it's annoying that this doesn't provide P-values. `nlme` package doesn't seem to work. 
pf(3.0681, df1 = 1, df2 = 5) #not significant

# try avg depth instead
m3 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3) # that's not happy either

#how about perimeter
m4 <- glmer(snag ~ stratum + perimeter + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m4) # no good

#pct_terrestrial_shore
m5 <- glmer(snag ~ stratum + pct_terrestrial_shore + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5) # nope

# Let's stop for now, conclude that we need to use a smaller land class division to fit the pool 4 model. 

#=============================
# POOL 8
#=============================
pool8_scaled <- allscaled[allscaled$pool == 8,] %>% dplyr::select(-pool)
# stratum only
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)

# start with the first two variables, stratum and wingdam
m1 <- glmer(snag ~ stratum + wingdam + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m1)
anova(m1)
pf(0.5735, 1, 5) #wingdam is not significant

# try avg_depth instead
m2 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m2)
anova(m2) #yes significant here
pf(5.2876, 1, 5) #not significant here

# is it better than the null model?
anova(m2, m0, test = "Chisq")
# marginally better

# try adding revetment
m3 <- glmer(snag ~ stratum + revetment + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3)

# that works. What about revetment and average depth?
m4 <- glmer(snag ~ stratum + revetment + avg_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m4)
anova(m4, m3, test = "Chisq") # barely not significant, marginal

# what about wing dam and average depth?
m5 <- glmer(snag ~ stratum + wingdam + avg_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5)
#compare to m1 and m2
anova(m5, m1, test = "Chisq") # yes
anova(m5, m2, test = "Chisq") # no

# stop here for now because the next variable is nearest land class
