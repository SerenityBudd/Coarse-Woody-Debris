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
# point-level GLM's based on variable importances from randomForest modeling
#==================================
# pool 13: substrt.p, current.p, stratum, depth.p, NEAR_TERR_DIST.p
# pool 8: stratum, NEAR_FOREST_DIST.p, depth.p, NEAR_TERR_CLASS_31.p, substrt.p
# pool 4: stratum, NEAR_TERR_CLASS_31.p, depth.p, NEAR_FOREST_DIST.p, NEAR_TERR_DIST.p
# all together: stratum, depth.p, substrt.p, (pool), NEAR_FOREST_DIST.p, NEAR_TERR_CLASS_31.p

head(all_reduced,3)
point <- all_reduced %>% dplyr::select(stratum, snag, pool, NEAR_TERR_DIST.p, NEAR_TERR_CLASS_31.p, NEAR_FOREST_DIST.p, NEAR_FOREST_CLASS_31.p, depth.p, current.p, substrt.p, wingdyke.p, riprap.p, trib.p) %>% filter(!is.na(snag)) #exclude stageht.p b/c too many NA's
head(point,3)
point$snag <- factor(as.character(point$snag))
str(point)
locate.nas(point)

#impute NA's
point <- rfImpute(x = point[,c(1, 3:ncol(point))], y = point$snag)
names(point)[1] <- "snag"

#separate into training and test data
nrow_Train <- 2*round(nrow(point)/3)
intrain <- sample(1:nrow(point), nrow_Train, replace = F)
train_point <- point[intrain,]
test_point <- point[-intrain,]

# GLM for all together:
pointmodel_minus <- glm(snag ~ current.p + pool + stratum + substrt.p + NEAR_FOREST_DIST.p + NEAR_TERR_DIST.p + depth.p + NEAR_FOREST_DIST.p*NEAR_TERR_DIST.p, data = point, family = "binomial")

pointmodel <- glm(snag ~ riprap.p + current.p + pool + stratum + substrt.p + NEAR_FOREST_DIST.p + NEAR_TERR_DIST.p + depth.p + NEAR_FOREST_DIST.p*NEAR_TERR_DIST.p, data = point, family = "binomial")

summary(pointmodel)
summary(pointmodel2)

cost<- function(r, pi = 0){
  mean(abs(r-pi) > 0.5)
}
pmcv <- cv.glm(data = point, glmfit = pointmodel, cost = cost, K = 1000) #cross-validate the model using leave-one-out cross-validation
str(pmcv)
pmcv_minus <- cv.glm(data = point, glmfit = pointmodel_minus, cost = cost, K = 100)#cross-validate the model using leave-one-out cross-validation
str(pmcv_minus)


pred <- predict.glm(pointmodel, newdata = test_point, type = "response")

# Separate others into training and test data and run models 
#separate out the pools
# Pool 4
point4 <- point %>% filter(pool == 4) %>% dplyr::select(-pool)
#separate into training and test data
nrow_Train <- 2*round(nrow(point4)/3)
intrain <- sample(1:nrow(point4), nrow_Train, replace = F)
train_point_4 <- point4[intrain,]
test_point_4 <- point4[-intrain,]
#make glm
pointmodel4 <- glm(snag ~ depth.p + stratum + depth.p*stratum + NEAR_FOREST_CLASS_31.p + NEAR_FOREST_DIST.p + NEAR_TERR_DIST.p, data = train_point_4, family = "binomial")
anova(pointmodel4)
summary(pointmodel4)
pred <- predict.glm(pointmodel4, newdata = test_point_4, type = "response")

# Pool 8
point8 <- point %>% filter(pool == 8) %>% dplyr::select(-pool)
#separate into training and test data
nrow_Train <- 2*round(nrow(point8)/3)
intrain <- sample(1:nrow(point8), nrow_Train, replace = F)
train_point_8 <- point8[intrain,]
test_point_8 <- point8[-intrain,]
#make glm
pointmodel8 <- glm(snag ~ stratum + NEAR_FOREST_DIST.p + depth.p + NEAR_TERR_CLASS_31.p + substrt.p, data = train_point_8, family = "binomial")
summary(pointmodel8)
pred <- predict.glm(pointmodel8, newdata = test_point_8, type = "response")

# Pool 13
point13 <- point %>% filter(pool == 13) %>% dplyr::select(-pool)
#separate into training and test data
nrow_Train <- 2*round(nrow(point13)/3)
intrain <- sample(1:nrow(point13), nrow_Train, replace = F)
train_point_13 <- point13[intrain,]
test_point_13 <- point13[-intrain,]
#make glm
pointmodel13 <- glm(snag ~ substrt.p + current.p + stratum + depth.p + NEAR_TERR_DIST.p + depth.p*stratum, data = train_point_13, family = "binomial")
summary(pointmodel13)
pred <- predict.glm(pointmodel13, newdata = test_point_13, type = "response")

step <- step(pointmodel13, direction  ="both")


#"What we do with the Roc to check for overfitting is to separete the dataset randomly in training and valudation and compare the AUC between those groups. If the AUC is "much" (there is also no rule of thumb) bigger in training then there might be overfitting." (https://stats.stackexchange.com/questions/71946/overfitting-a-logistic-regression-model)
