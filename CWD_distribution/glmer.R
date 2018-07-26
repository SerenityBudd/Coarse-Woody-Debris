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
anova(m1, m0, test = "Chisq") #good

# add pool
m2 <- glmer(snag ~ stratum + avg_depth + pool + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m2)
anova(m2, m1, test = "Chisq") # very good

# add revetment
m3 <- glmer(snag ~ stratum + avg_depth + pool + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3)
anova(m3, m2, test = "Chisq") # still good

# add nearest_land_class
m4 <- glmer(snag ~ stratum + avg_depth + pool + revetment + nearest_land_class + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "Nelder_Mead",
                                   optCtrl = list(maxfun = 2e5)))
# failed to converge

# Back to m3. Try interacting stratum and avg_depth
m5 <- glmer(snag ~ stratum*avg_depth + pool + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5) #mistakes have been made

# What if we interact avg_depth with pool instead?
m6 <- glmer(snag ~ stratum + avg_depth*pool + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m6) # nope
anova(m6) # nope

# Last try: interact avg_depth with revetment
m7 <- glmer(snag ~ stratum + pool + avg_depth*revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m7) # ooh
anova(m7) #hmm
pf(99.666, 1, 11)
pf(6.403, 1, 11)
pf(12.545, 1, 11)
pf(42.466, 2, 11)
pf(66.254, 6, 11)
#... guess not.


#=============================
# POOL 4
#=============================
pool4_scaled <- allscaled[allscaled$pool == 4,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add nearest_land_class
m1 <- glmer(snag ~ stratum + nearest_land_class + (1 | uniq_id),
      data = pool4_scaled,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl=list(maxfun=2e5))) 
summary(m1) #doesn't work

# that didn't work; probably too many categories. Try wingdam instead.
m2 <- glmer(snag ~ stratum + wingdam + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m2)
anova(m2, m0, test = "Chisq") #not good

# try perimeter instead
m3 <- glmer(snag ~ stratum + perimeter + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m3) # not good

# try avg_depth instead
m4 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m4) # also not good

# this one is just a disaster; i don't know what to do. 

#=============================
# POOL 8
#=============================
pool8_scaled <- allscaled[allscaled$pool == 8,] %>% dplyr::select(-pool)
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# start with the first two variables, stratum and wingdam
m1 <- glmer(snag ~ stratum + wingdam + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m1)
# that was bad
anova(m1, m0, test = "Chisq") #not significant

# how about stratum and average depth
m2 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m2)
anova(m2, m0, test = "Chisq") #marginally significant

# add revetment
m3 <- glmer(snag ~ stratum + avg_depth + revetment + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m3) #not good

# add shoreline density index
m4 <- glmer(snag ~ stratum + avg_depth + shoreline_density_index + (1 | uniq_id),
                  data = pool8_scaled,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5))) 
summary(m4)
anova(m4, m2, test = "Chisq") # marginally significant

# add interaction term between stratum and avg_depth
m5 <- glmer(snag ~ stratum*avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m5)
anova(m5) #very bad

# add interaction term instead between stratum and shoreline_density_index
m6 <- glmer(snag ~ avg_depth + stratum*shoreline_density_index + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(m6)
anova(m6) #very very very bad

mfinal_pool8 <- m4
summary(mfinal_pool8)

#=============================
# POOL 13
#=============================
pool13_scaled <- allscaled[allscaled$pool == 13,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1)
anova(m1, m0, test = "Chisq") #marginally helpful

# add shoreline_density_index
m2 <- glmer(snag ~ stratum + avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) #nope nope nope

# add max depth instead
m3 <- glmer(snag ~ stratum + avg_depth + max_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) #nope nope nope

# The best model we had was m1, which had stratum and avg depth. What if we interact the two?
m4 <- glmer(snag ~ stratum*avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4)
anova(m4)
pf(4.20, 5, 11)
pf(4.30, 1, 11)
# summary looks okay but neither of these terms is significant in the anova. 

