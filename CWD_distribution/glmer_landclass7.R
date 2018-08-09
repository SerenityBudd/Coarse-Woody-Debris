library(lme4)
source("libraries.R")
library(effects)
load("data/all_reduced_7.Rda")
all_reduced <- all_reduced_7 %>% dplyr::select(-c(barcode, Area, pct_prm_lotic, pct_prm_lentic, num_lotic_outl, num_lentic_outl, econ, pct_terr_shore_wetf, sinuosity, NEAR_TERR_CLASS_7_N.p, NEAR_FOREST_CLASS_7_N.p, year.p, stageht.p, stratum_name, snagyn, pct_area_le100, depth.p, current.p, substrt.p, trib.p, pct_aqveg, AQUA_CODE)) %>% na.omit()
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

table(allscaled$nearest_land_class)
# only one row has "Ag" as the nearest land class, so let's get rid of that row.
allscaled <- allscaled[allscaled$nearest_land_class != "Ag",]
table(allscaled$nearest_forest_class) # only one class, so this isn't a good predictor
allscaled$nearest_forest_class <- NULL # remove nearest_forest_class

#=============================
# ALL POOLS
#=============================
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m1)
plot(allEffects(m1), rescale.axis = F)

# add revetment
m2 <- glmer(snag ~ stratum + avg_depth + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m2)
Anova(m2, type = 2)
anova(m2, m1, test = "Chisq") # ok

# add nearest_land_class
m3 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3)
Anova(m3, type = 2)
anova(m3, m2, test = "Chisq") # good good

# add pct_perimeter_wetforest
m4 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + pct_perimeter_wetforest + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m4)
Anova(m4, type = 2)
anova(m4, m3, test = "Chisq") #marginal
plot(allEffects(m4, rescale.axis = F))

# add wingdam
m5 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + pct_perimeter_wetforest + wingdam + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5)
Anova(m5, type = 2)
anova(m5, m4, test = "Chisq") #nope

# This is too complicated. Let's go back to m0, which will just show differences between the habitat strata. 
allscaled <- within(allscaled, stratum <- relevel(stratum, ref = "MCB-U"))
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)
plot(allEffects(m0),
     main = "Probability of large wood by habitat type",
     xlab = "Habitat type",
     ylab = "P(wood)",
     rescale.axis = F,
     ylim = c(0,1))

# do we want to look at depth or something else broken down by stratum, or just abandon stratum entirely?
