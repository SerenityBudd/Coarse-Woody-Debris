# Make null two-level model
fit <- glmer(snag ~ (1|uniq_id), family = binomial("logit"), data = sites_aa_5m)
summary(fit)


coefs <- lme4::ranef(fit, condVar = TRUE)
coefs_se <- sqrt(attr(coefs[[1]], "postVar")[1, ,]) #even though postVar is deprecated, the name of the attribute is still postVar, not condVar. Go figure. 
polyids <- rownames(coefs[[1]])
coefstable <- cbind("polyid" = polyids, "coef" = coefs[[1]], "coef_se" = coefs_se)
row.names(coefstable) <- NULL
colnames(coefstable)[2] <- "coef" #for some reason it didn't properly name the column the first time around
coefstable <- coefstable[order(coefstable$coef),]
coefstable$coefrank <- 1:nrow(coefstable)

  #make a plot of the rank-ordered polygons
with(coefstable, plot(coef~coefrank, type = "n", main = "Rank-Ordered Plot of Random Effects Coefficients for Aquatic Polygons", ylab = "Random Effect", xlab = "Aquatic Polygon (ranked)"))
segments(coefstable$coefrank, coefstable$coef - 1.96*coefstable$coef_se, coefstable$coefrank, coefstable$coef + 1.96*coefstable$coef_se)
points(coefstable$coefrank, coefstable$coef, col = "blue", pch = 18)
abline(h = 0, col = "red")

#Making a mixed-effects model with `uniq_id` as a random effect variable
mixed.ef <- glmer(snag ~ depth.p + NEAR_TERR_DIST.p + NEAR_TERR_CLASS_31.p + stratum + (1|uniq_id), data = sites_aa_5m, family = binomial("logit"))
  #ok so this works and gives an output. also gives lots of warning messages and I don't really know what they mean...

#==================================
# polygon-level GLM's based on variable importances from randomForest modeling
#==================================
# pool 4: pct_aqveg, pct_area_le100, avg_depth, Area, tot_vol, max_depth
# pool 8: tot_vol, avg_depth, riprap, pct_area_le100, AQUA_CODE, pct_terr
# pool 13: median_current.p, AQUA_CODE, riprap, wingdyke, [near terr dist, pct_prm_wetf, pct_terr]
# all together: pool, pct_prm_wetf, avg_depth, pct_terr_shore_wetf, max_depth, Perimeter, median_current.p

load("data/poly.Rda")
source("libraries.R")
source("ownfunctions.R")
locate.nas(poly)
poly <- na.omit(poly) #for now, we'll just drop the rows that have NA's in them. But I don't understand why the depth information is missing, and this may significantly affect the models. Can we fill in the missing data?

poly$nyes <- poly$propsnag*poly$n
poly$nno <- poly$n - poly$nyes

poly <- poly %>% dplyr::select(-c(uniq_id, n))
#=================================================
# GLM for all together:
  #seems redundant to include both pct_prm_wetf and pct_terr_shore_wetf
m1 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth, data = poly, family = "binomial")
summary(m1)
anova(m1, test = "Chisq")

# Try adding max_depth
m2 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + max_depth, data = poly, family = "binomial")
summary(m2)
anova(m2, test = "Chisq") #not great; let's test against m1.
anova(m2, m1, test = "Chisq") #not super great, and combined with the fact that we already have avg_depth, let's move on.

# try Perimeter
m3 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter, data = poly, family = "binomial")
summary(m3)
anova(m3, test = "Chisq")
anova(m3, m1, test = "Chisq") 
  #ok, that's helpful. 

# try current?
m4 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + median_current.p, data = poly, family = "binomial")
summary(m4)
anova(m4, test = "Chisq")
anova(m4, m3, test = "Chisq") 
  #marginally helpful but probably not

# try riprap?
m5 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap, data = poly, family = "binomial")
summary(m5)
anova(m5, test = "Chisq")
anova(m5, m3, test = "Chisq") 
  # that seems to help

# Try AQUA_CODE
m6 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap + AQUA_CODE, data = poly, family = "binomial")
summary(m6)
anova(m6, test = "Chisq")
anova(m6, m5, test = "Chisq") 
  # This helps a lot!

# Try Area
m7 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap + AQUA_CODE + Area, data = poly, family = "binomial")
summary(m7)
anova(m7, test = "Chisq")
Anova(m7, test.statistic = "F", type = "II")
anova(m7, m6, test = "Chisq") 
  # Pretty clearly not helpful

# Try forest distance
m8 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap + AQUA_CODE + medianNEAR_FOREST_DIST.p, data = poly, family = "binomial")
summary(m8)
anova(m8, test = "Chisq")
Anova(m8, test.statistic = "Wald", type = "II")
anova(m8, m6, test = "Chisq") 
  # Marginally helpful but probably not justifiable. 

# Add interaction between riprap and pct_prm_wetf (leaving out forest dist for now)
m9 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap + AQUA_CODE + pct_prm_wetf*riprap, data = poly, family = "binomial")
summary(m9)
anova(m9, test = "Chisq")
anova(m9, m6, test = "Chisq") 
  #NOPENOPENOPE

# Add interaction between AQUA_CODE and avg_depth (leaving out forest dist for now)--motivated by the stratum*depth logistic regression from a while ago (on the point level)
m10 <- glm(cbind(nyes, nno) ~ pool + pct_prm_wetf + avg_depth + Perimeter + riprap + AQUA_CODE + AQUA_CODE*avg_depth, data = poly, family = "binomial")
summary(m10)
anova(m10, test = "Chisq")
Anova(m10, test.statistic = "Wald", type = "II")
anova(m10, m6, test = "Chisq") 
  # This seems helpful.

mod_all <- m10
summary(mod_all)
save(mod_all, file = "data/mod_all.Rda")
#=================================================
# GLM for pool 4 only
m1 <- glm(cbind(nyes, nno) ~ pct_aqveg + pct_area_le100 + avg_depth + Area + tot_vol + max_depth, data = poly[poly$pool == 4,], family = "binomial")
summary(m1)
anova(m1, test = "Chisq")
  # we should get rid of tot_vol and max_depth

m2 <- glm(cbind(nyes, nno) ~ pct_aqveg + pct_area_le100 + avg_depth + Area, data = poly[poly$pool == 4,], family = "binomial")
summary(m2)
anova(m2, test = "Chisq")
anova(m2, m1, test = "Chisq") #not justifiable to add those variables
  # pct_aqveg is looking questionable, and in fact, our variables all seem pretty heavily related to each other:
  pairs(poly[poly$pool == 4, c("pct_aqveg", "pct_area_le100", "avg_depth")])
  # run PCA on these variables
  pca <- princomp(scale(poly[,c("pct_aqveg", "pct_area_le100", "avg_depth", "tot_vol", "max_depth")]))
  plot(pca$scores)
  # depth is definitely one component, and pct_aqveg is another. Let's get rid of either avg_depth or pct_area_le100.
  
# Remove pct_area_le100
m3 <- glm(cbind(nyes, nno) ~ pct_aqveg + avg_depth + Area, data = poly[poly$pool == 4,], family = "binomial")
summary(m3)
anova(m3, test = "Chisq")
anova(m3, m2, test = "Chisq")
  # it doesn't like that: it is justifiable to add pct_area_le100 back to the model. Try the opposite

# Remove avg_depth
m4 <- glm(cbind(nyes, nno) ~ pct_aqveg + pct_area_le100 + Area, data = poly[poly$pool == 4,], family = "binomial")
summary(m4)
anova(m4, test = "Chisq")
anova(m4, m2, test = "Chisq") # justifiable to add this back. Guess we're back to mod2. 

# Remove pct_aqveg?
m5 <- glm(cbind(nyes, nno) ~ avg_depth + pct_area_le100 + Area, data = poly[poly$pool == 4,], family = "binomial")
summary(m5)
anova(m5, test = "Chisq")
anova(m5, m2, test = "Chisq") #not justifiable to add pct_aqveg back. This is now our working model. 

# Add Perimeter
m6 <- glm(cbind(nyes, nno) ~ avg_depth + pct_area_le100 + Area + Perimeter, data = poly[poly$pool == 4,], family = "binomial")
summary(m6)
anova(m6, test = "Chisq")
anova(m6, m5, test = "Chisq")
  # that was definitely not the right choice. 

# Add shoreline_density_index instead
m7 <- glm(cbind(nyes, nno) ~ avg_depth + pct_area_le100 + Area + shoreline_density_index, data = poly[poly$pool == 4,], family = "binomial")
summary(m7)
anova(m7, test = "Chisq")
anova(m7, m5, test = "Chisq")
  # that was definitely not the right choice either. Back to m5. 

# Add interaction between Area and avg_depth
m8 <- glm(cbind(nyes, nno) ~ avg_depth + pct_area_le100 + Area + avg_depth*Area, data = poly[poly$pool == 4,], family = "binomial")
summary(m8)
anova(m8, test = "Chisq")
anova(m8, m5, test = "Chisq")
  # again, definitely not a good idea. Let's stay with model 5 for now

mod_pool_4 <- m5
summary(mod_pool_4)

#=================================================
# GLM for pool 8 only
m1 <- glm(cbind(nyes, nno) ~ tot_vol + avg_depth + riprap + pct_area_le100 + AQUA_CODE, data = poly[poly$pool == 8,], family = "binomial")
summary(m1)
anova(m1, test = "Chisq")
  # Let's try getting rid of tot_vol and avg_depth

m2 <- glm(cbind(nyes, nno) ~ riprap + pct_area_le100 + AQUA_CODE, data = poly[poly$pool == 8,], family = "binomial")
summary(m2)
anova(m2, test = "Chisq")
anova(m2, m1, test = "Chisq")
  # Something could be added back: try adding back avg_depth?

m3 <- glm(cbind(nyes, nno) ~ riprap + pct_area_le100 + AQUA_CODE + avg_depth, data = poly[poly$pool == 8,], family = "binomial")
summary(m3)
anova(m3, test = "Chisq")
anova(m3, m2, test = "Chisq")
  # Nope, that wasn't the right move. What if we substitute avg_depth for pct_area_le100?

m4 <- glm(cbind(nyes, nno) ~ riprap + avg_depth + AQUA_CODE, data = poly[poly$pool == 8,], family = "binomial")
summary(m4)
anova(m4, test = "Chisq")
  # Still not good. Back to m2. 

# Add pct_terr
m5 <- glm(cbind(nyes, nno) ~ riprap + pct_area_le100 + AQUA_CODE + pct_terr, data = poly[poly$pool == 8,], family = "binomial")
summary(m5)
anova(m5, test = "Chisq")
anova(m5, m2, test = "Chisq")
  # this is a bit better but it's not great

# feel like I'm flailing in the dark with pool 8 here. Consult a biologist?

#=================================================
# GLM for pool 13 only
m1 <- glm(cbind(nyes, nno) ~ median_current.p + AQUA_CODE + riprap, data = poly[poly$pool == 13,], family = "binomial")
summary(m1)
anova(m1, test = "Chisq")
  # let's try interacting riprap.p with median_current.p instead

# Interact riprap.p with median_current.p
m2 <- glm(cbind(nyes, nno) ~ median_current.p*riprap + AQUA_CODE, data = poly[poly$pool == 13,], family = "binomial")
summary(m2)
anova(m2, test = "Chisq")
  # that was a very bad idea.

# Interact riprap.p with AQUA_CODE instead.
m3 <- glm(cbind(nyes, nno) ~ AQUA_CODE*riprap + median_current.p, data = poly[poly$pool == 13,], family = "binomial")
summary(m3)
anova(m3, test = "Chisq")
  # that was also a very bad idea. I guess we're just getting rid of riprap.p.

# Remove ripap.p entirely
m3 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p, data = poly[poly$pool == 13,], family = "binomial")
summary(m3)
anova(m3, test = "Chisq")
  # that's better. Now what else can we add?

# Add wingdyke
m4 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + wingdyke, data = poly[poly$pool == 13,], family = "binomial")
summary(m4)
anova(m4, test = "Chisq")
anova(m3, m4, test = "Chisq") #can't justify adding this
  # nope. Let's try NEAR_TERR_DIST.p instead

# Add NEAR_TERR_DIST.p
m5 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + medianNEAR_TERR_DIST.p, data = poly[poly$pool == 13,], family = "binomial")
summary(m5)
anova(m5, test = "Chisq")
anova(m5, m3, test = "Chisq")
  # this is a little more promising, but not hugely

# Add pct_prm_wetf instead
m6 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + pct_prm_wetf, data = poly[poly$pool == 13,], family = "binomial")
summary(m6)
anova(m6, test = "Chisq")
anova(m6, m3, test = "Chisq")
  # nope, that didn't work.

# Add pct_terr instead
m7 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + pct_terr, data = poly[poly$pool == 13,], family = "binomial")
summary(m7)
anova(m7, test = "Chisq")
anova(m7, m3, test = "Chisq")
  # that's a lot better!

# Add interaction between AQUA_CODE and pct_terr
m8 <- glm(cbind(nyes, nno) ~ AQUA_CODE*pct_terr + median_current.p, data = poly[poly$pool == 13,], family = "binomial")
summary(m8)
anova(m8, test = "Chisq")
anova(m8, m7, test = "Chisq")
  # barely justifiable

# Instead, add max_depth
m9 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + pct_terr + max_depth, data = poly[poly$pool == 13,], family = "binomial")
summary(m9)
anova(m9, test = "Chisq")
anova(m9, m7, test = "Chisq")
  # looks okay, but what if we did avg_depth instead?

# Add avg_depth instead of max_depth
m10 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p + pct_terr + avg_depth, data = poly[poly$pool == 13,], family = "binomial")
summary(m10)
anova(m10, test = "Chisq")
anova(m10, m7, test = "Chisq")
  # this is not good. Revert to model 9

# Could we interact max_depth and current?
m11 <- glm(cbind(nyes, nno) ~ AQUA_CODE + median_current.p*max_depth + pct_terr, data = poly[poly$pool == 13,], family = "binomial")
summary(m11)
anova(m11, test = "Chisq")
anova(m11, m9, test = "Chisq")
  # good instinct. 

# Interact AQUA_CODE with pct_terr
m12 <- glm(cbind(nyes, nno) ~ AQUA_CODE*pct_terr + median_current.p*max_depth, data = poly[poly$pool == 13,], family = "binomial")
summary(m12)
anova(m12, test = "Chisq")
anova(m12, m11, test = "Chisq")
  # barely better; I'd say it's not justified.

# Instead, interact AQUA_CODE with median_current.p
m13 <- glm(cbind(nyes, nno) ~ AQUA_CODE*median_current.p + median_current.p*max_depth + pct_terr, data = poly[poly$pool == 13,], family = "binomial")
summary(m13)
anova(m13, test = "Chisq")
anova(m13, m11, test = "Chisq")
  #NOPENOPENOPE

# Instead, interact AQUA_CODE with max_depth
m14 <- glm(cbind(nyes, nno) ~ AQUA_CODE*max_depth + median_current.p*max_depth + pct_terr, data = poly[poly$pool == 13,], family = "binomial")
summary(m14)
anova(m14, test = "Chisq")
anova(m14, m11, test = "Chisq")
  # this isn't great, but what if we took out median_current.p*max_depth and just leave those in on their own?

# Remove the median_current.p*max_depth term but keep the AQUA_CODE*max_depth interaction
m15 <- glm(cbind(nyes, nno) ~ AQUA_CODE*max_depth + median_current.p + pct_terr, data = poly[poly$pool == 13,], family = "binomial")
summary(m15)
anova(m15, test = "Chisq")
anova(m15, m9, test = "Chisq")
  # looks like this is slightly better than median_current.p*max_depth.

#we'll go with this model for now. 
mod_pool_13 <- m15
summary(mod_pool_13)

