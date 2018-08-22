
#========================================
# cforest tree on point level for larger sample size
#========================================
source("libraries.R")
source("ownfunctions.R")
# Upper river
load("data/all_reduced_clean.Rda")
dim(all_reduced_clean)
locate.nas(all_reduced_clean)

# Lower river
load("data/all2_reduced_clean.Rda")
dim(all2_reduced_clean)
locate.nas(all2_reduced_clean)

# Bind the two datasets together
combined <- rbind(all_reduced_clean, all2_reduced_clean)
combined$snag <- factor(as.character(combined$snag))
combined$wingdyke <- factor(as.character(combined$wingdyke))
combined$riprap <- factor(as.character(combined$riprap))

# Remove some variables that we aren't going to use
combined <- combined %>% dplyr::select(-c(barcode, lcode, sdate, utm_e, utm_n, year, near_terr_name))

# Scale the numeric variables to avoid problems with model convergence. 
combined_scaled <- combined %>%
  mutate_if(is.numeric, scale) # so elegant!!!
#=============================
# ALL POOLS
#=============================
m0 <- glmer(snag ~ wingdam + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ wingdam + avg_depth + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m1)

# add revetment
m2 <- glmer(snag ~ wingdam + avg_depth + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m2)
anova(m2)
anova(m2, m1, test = "Chisq") #ooh, nice

# add dist_to_forest
m3 <- glmer(snag ~ wingdam + avg_depth + revetment + dist_to_forest + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3)
anova(m3, m2, test = "Chisq") # good good

# add nearest_land_class
m4 <- glmer(snag ~ wingdam + avg_depth + revetment + dist_to_forest + nearest_land_class + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m4)
Anova(m4, type = 2)
anova(m4, m3, test = "Chisq") # good good

# add shoreline density index
m5 <- glmer(snag ~ wingdam + avg_depth + revetment + dist_to_forest + nearest_land_class + shoreline_density_index + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5)
anova(m5, m4, test = "Chisq") #marginal

# back to m4. Interact revetment and dist_to_forest; get rid of shoreline_density_index.
m6 <- glmer(snag ~ wingdam + avg_depth + revetment*dist_to_forest + nearest_land_class + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m6)
Anova(m6, type = 2)
plot(allEffects(m6))
wd <- allEffects(m6)[1]
ad <- allEffects(m6)[2]
nlc <- allEffects(m6)[3]
rbydtf <- allEffects(m6)[4]

plot(wd,
     rescale.axis = F,
     ylim = c(0,1),
     xlab = "Wingdam",
     ylab = "P(wood)")
plot(nlc,
     rescale.axis = F,
     ylim = c(0,1),
     ylab = "P(wood)")
plot(rbydtf,
     rescale.axis = F,
     ylim = c(0,1),
     xlab =  "Distance to nearest forest (scaled)",
     ylab = ("P(wood)"))

# interact revetment by nearest land class instead
m7 <- glmer(snag ~ wingdam + avg_depth + revetment*nearest_land_class + dist_to_forest + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m7)
Anova(m7, type = 2)
#plot(allEffects(m7))


#I think revetment by dist_to_forest is more informative. 
mfinal_all_nostratum <- m6
summary(mfinal_all_nostratum)
save(mfinal_all_nostratum, file = "data/mfinal_all_nostratum.Rda")
save(allscaled, file = "data/allscaled.Rda")
#=============================
# POOL 4
#=============================
pool4_scaled <- allscaled[allscaled$pool == 4,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ wingdam + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add nearest land class
m1 <- glmer(snag ~ wingdam + nearest_land_class + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1)
Anova(m1, type = 2)

# add avg_depth
m2 <- glmer(snag ~ wingdam + nearest_land_class + avg_depth + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) # ok good
Anova(m2, type = 2)

# add dist_to_forest
m3 <- glmer(snag ~ wingdam + nearest_land_class + avg_depth + dist_to_forest + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) # still good
Anova(m3, type = 2)

# add perimeter
m4 <- glmer(snag ~ wingdam + nearest_land_class + avg_depth + dist_to_forest + perimeter + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) # nope, stop here
Anova(m4, type = 2)

# interact wingdam by avg_depth: not good
# interact nearest_land_class by avg_depth: not good
# interact wingdam by dist_to_forest: not good
# interact wingdam by nearest_land_class: not good
# interact nearest_land_class by dist_to_forest: not good
# interact dist_to_forest by avg_depth: not good
# ok, I guess we're not gonna use any interactions for this one.

mfinal_pool4_nostratum <- m3
plot(allEffects(m3))
summary(mfinal_pool4_nostratum)

#=============================
# POOL 8
#=============================
pool8_scaled <- allscaled[allscaled$pool == 8,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ wingdam + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ wingdam + avg_depth + (1 | uniq_id),
           data = pool8_scaled,
           family = binomial,
           control = glmerControl(optimizer = "bobyqa",
                                  optCtrl = list(maxfun=2e5)))
summary(m1) #looks ok
anova(m1, m0, test = "Chisq") #looks good

# add shoreline_density_index
m2 <- glmer(snag ~ wingdam + avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) #marginal
anova(m2, m1, test = "Chisq") #barely good
Anova(m2, type = 2)

# try nearest_land_class instead
m3 <- glmer(snag ~ wingdam + avg_depth + nearest_land_class + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) #marginal
Anova(m3, type = 2) #ok

# add dist_to_forest
m4 <- glmer(snag ~ wingdam + avg_depth + nearest_land_class + dist_to_forest + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) #ok
Anova(m4, type = 2) #ok

# add max_depth
m5 <- glmer(snag ~ wingdam + avg_depth + nearest_land_class + dist_to_forest + max_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5) # nope
Anova(m5, type = 2) # nope nope nope

# add revetment instead
m6 <- glmer(snag ~ wingdam + avg_depth + nearest_land_class + dist_to_forest + revetment + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6) # hmm
Anova(m6, type = 2) # looks good, but now I don't like avg_depth

# remove avg_depth
m7 <- glmer(snag ~ wingdam + nearest_land_class + dist_to_forest + revetment + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m7) # ok
Anova(m7, type = 2) #looks better

# interact revetment by dist_to_forest
m8 <- glmer(snag ~ wingdam + nearest_land_class + dist_to_forest*revetment + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m8) # ok
Anova(m8, type = 2) #looks ok
plot(allEffects(m8))

# interact revetment*nearest_land_class: doesn't work
# interact wingdam*dist_to_forest: only marginally significant
# interact dist_to_forest*nearest_land_class: only marginally significant

mfinal_pool8_nostratum <- m8

#=============================
# POOL 13
#=============================
pool13_scaled <- allscaled[allscaled$pool == 13,] %>% dplyr::select(-pool)

# start with avg_depth
m0 <- glmer(snag ~ avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# weird, not sure why that's not significant. Maybe wingdam instead?
m1 <- glmer(snag ~ wingdam + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1) # that's better. What if we add avg_depth on top of that?

# add avg_depth
m2 <- glmer(snag ~ wingdam + avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) #marginally significant

# add shoreline_density_index
m3 <- glmer(snag ~ wingdam + avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) # not good. What if we try it without avg_depth?
anova(m3, m2, test = "Chisq") #badbad

# add shoreline_density_index but remove avg_depth
m4 <- glmer(snag ~ wingdam + shoreline_density_index + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) # not good at all. 

# back to model 2. Add max_depth
m5 <- glmer(snag ~ wingdam + avg_depth + max_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5) #whoa, very bad

# add pct_terrestrial shore instead
m6 <- glmer(snag ~ wingdam + avg_depth + pct_terrestrial_shore + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6) # not good

# okay, I guess we're sticking with model 2. One last thing: try interacting wingdam with avg_depth. 
m7 <- glmer(snag ~ wingdam*avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m7) # not good
Anova(m7, type = 2)
Anova(m7, type = 3)
# not good

# try adding dist_to_land
m8 <- glmer(snag ~ wingdam + avg_depth + dist_to_land + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m8) #ooh, that looks nice
anova(m8, m2, test = "Chisq")

# adding perimeter gives nothing interesting
# add revetment
m9 <- glmer(snag ~ wingdam + avg_depth + dist_to_land + revetment + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m9) #ooh, that looks nice
anova(m9, m8, test = "Chisq")

# add dist_to_forest: marginal
# interact revetment*dist_to_land: nope nope
# interact revetment*wingdam: very bad
# interact revetment*avg_depth: ok, but some marginal effects
# let's stuck with m9. 
plot(allEffects(m9))

mfinal_pool13_nostratum <- m9
summary(mfinal_pool13_nostratum)

finalmodels_nostratum <- list(mfinal_all_nostratum, mfinal_pool4_nostratum, mfinal_pool8_nostratum, mfinal_pool13_nostratum)
save(finalmodels_nostratum, file = "data/finalmodels_nostratum.Rda")

datasets_nostratum <- list(allscaled, pool4_scaled, pool8_scaled, pool13_scaled)
names(datasets_nostratum) <- c("allscaled", "pool4_scaled", "pool8_scaled", "pool13_scaled")
save(datasets_nostratum, file = "data/datasets_nostratum.Rda")

# Make nice effect plots for the all pools model
plot(wd, main = "Wood by wingdam presence", 
     xlab = "Wingdam presence (0 = no, 1 = yes)", 
     ylab = "P(wood)", ylim = c(-2, 2))
plot(ad, main = "Wood by avg. water depth",
     xlab = "Avg. water depth, scaled (sd from mean)",
     ylab = "P(wood)", ylim = c(-2, 2))
plot(nlc, main = "Wood by nearest landcover type",
     xlab = "Nearest landcover type",
     ylab = "P(wood)", ylim = c(-2, 2))
plot(rbydtf, ylim = c(-2, 2))

