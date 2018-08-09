
#========================================
# cforest tree on point level for larger sample size
#========================================
source("libraries.R")
load("data/all_reduced_7.Rda")
all_reduced <- all_reduced_7 %>% dplyr::select(-c(stratum, barcode, uniq_id, Area, pct_prm_lotic, pct_prm_lentic, num_lotic_outl, num_lentic_outl, econ, pct_terr_shore_wetf, sinuosity, NEAR_TERR_CLASS_7_N.p, NEAR_FOREST_CLASS_7_N.p, year.p, stageht.p, stratum_name, snagyn, pct_area_le100, depth.p, current.p, substrt.p, trib.p, pct_aqveg, AQUA_CODE)) %>% na.omit()
all <- all_reduced
colnames(all) <- c("snag", "pool", "perimeter", "max_depth", "avg_depth", "total_volume", "shoreline_density_index", "pct_terrestrial_shore", "pct_perimeter_wetforest", "dist_to_land", "nearest_land_class", "dist_to_forest", "nearest_forest_class", "wingdam", "revetment")
all$snag <- factor(as.character(all$snag))
table(all$nearest_forest_class)
#remove this factor
all$nearest_forest_class <- NULL
table(droplevels(all$nearest_land_class))
all <- all %>% filter(nearest_land_class != "Ag") #don't need this anymore

#=========================
# intialize functions
#=========================

# initialize function to make nicer variable importance plot
plotnice <- function(varimps, color, title){
  ggplot(data = varimps, aes(x = predictorvars, y = imps))+
    geom_col(fill = color)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x=element_blank())+
    ylab("Rel. importance")+
    ggtitle(title)+
    theme(text = element_text(size=20))
}

# initialize function for splitting data into cross-validation sets
splitup <- function(df, k){
  set.seed(1)
  inds <- sample(rep(1:k, length = nrow(df)))
  df$grp <- inds
  return(df)
}

#=================================
# Points, all pools 
#=================================

# split the full data set into 5 groups
all <- splitup(all, 5)

# define predictor variables for the full data set
predictorvars_all <- c("perimeter", "max_depth", "avg_depth", "total_volume", "shoreline_density_index", "pct_terrestrial_shore", "pct_perimeter_wetforest", "dist_to_land", "nearest_land_class", "dist_to_forest", "wingdam", "revetment")

# initialize an empty data frame to contain variable importance values
importances_all <- data.frame(
  predictorvars_all
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- all %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable
  test <- all %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp``
  
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag ~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights come from the model.
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_all[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_all_landclass7nostratum <- mean(ers) # cross-validated mean error rate

imps_all <- rowSums(importances_all)/5 # calculate average variable importances
varimps_all_nostratum <- cbind(predictorvars_all, imps_all) %>% as.data.frame %>% mutate(imps_all = as.numeric(as.character(imps_all))) %>% mutate(predictorvars_all = reorder(predictorvars_all, -imps_all))
colnames(varimps_all_nostratum) <- c("predictorvars", "imps")# get the data into the right format for ggplot
save(varimps_all_nostratum, file = "data/varimps_all_nostratum.Rda") #save the variable importances

p <- plotnice(varimps_all_nostratum, color = "olivedrab4", title = "Point RF var. importance (5fold), all pools")
ggsave(plot = p, "rf_avg_varimp_point_all_5foldcv_landclass7nostratum.png",
       width = 7, height = 4.5)
save(cv_error_all_landclass7nostratum, file = "data/cv_error_all_landclass7nostratum.Rda")
# save as rf_avg_varimp_point_all_5foldcv.png

#=============================
# Points, individual pools 
#=============================

predictorvars_pools <- c("perimeter", "max_depth", "avg_depth", "total_volume", "shoreline_density_index", "pct_terrestrial_shore", "pct_perimeter_wetforest", "dist_to_land", "nearest_land_class", "dist_to_forest", "wingdam", "revetment") #define vars for the pools

#=============================
# pool 4

points_4 <- splitup(all[all$pool == 4,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool4 <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_4 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.  
  test <- points_4 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`..
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool4[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool4_landclass7nostratum <- mean(ers) # cross-validated mean error rate

imps_pool4 <- rowSums(importances_pool4)/5 # calculate average variable importances
varimps_pool4_nostratum <- cbind(predictorvars_pools, imps_pool4) %>% as.data.frame %>% mutate(imps_pool4 = as.numeric(as.character(imps_pool4))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool4)) # get the data into the right format for ggplot
colnames(varimps_pool4_nostratum) <- c("predictorvars", "imps")
save(varimps_pool4_nostratum, file = "data/varimps_pool4_nostratum.Rda") #save the variable importances

p4 <- plotnice(varimps_pool4_nostratum, color = "cadetblue4", title = "Point RF var. importance (5fold), pool 4")
# save as rf_avg_varimp_point_pool4_5foldcv.png
ggsave(plot = p4, 
       "rf_avg_varimp_point_pool4_5foldcv_landclass7nostratum.png",
       width = 7, height = 4.5)
save(cv_error_pool4_landclass7nostratum, file = "data/cv_error_pool4_landclass7nostratum")

#=============================
# pool 8

points_8 <- splitup(all[all$pool == 8,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool8 <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_8 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_8 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool8[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool8_landclass7nostratum <- mean(ers) # cross-validated mean error rate

imps_pool8 <- rowSums(importances_pool8)/5 # calculate average variable importances
varimps_pool8_nostratum <- cbind(predictorvars_pools, imps_pool8) %>% as.data.frame %>% mutate(imps_pool8 = as.numeric(as.character(imps_pool8))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool8)) # get the data into the right format for ggplot
colnames(varimps_pool8_nostratum) <- c("predictorvars", "imps")
save(varimps_pool8_nostratum, file = "data/varimps_pool8_nostratum.Rda") #save the variable importances

p8 <- plotnice(varimps_pool8_nostratum, color = "brown", title = "Point RF var. importance (5fold), pool 8")
# save as rf_avg_varimp_point_pool8_5foldcv.png
ggsave(plot = p8, 
       "rf_avg_varimp_point_pool8_5foldcv_landclass7nostratum.png",
       width = 7, height = 4.5)
save(cv_error_pool8_landclass7nostratum, file = "data/cv_error_pool8_landclass7nostratum.Rda")

#=============================
# pool 13

points_13 <- splitup(all[all$pool == 13,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool13 <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_13 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_13 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool13[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool13_landclass7nostratum <- mean(ers) # cross-validated mean error rate

imps_pool13 <- rowSums(importances_pool13)/5 # calculate average variable importances
varimps_pool13_nostratum <- cbind(predictorvars_pools, imps_pool13) %>% as.data.frame %>% mutate(imps_pool13 = as.numeric(as.character(imps_pool13))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool13)) # get the data into the right format for ggplot
colnames(varimps_pool13_nostratum) <- c("predictorvars", "imps")
save(varimps_pool13_nostratum, file = "data/varimps_pool13_nostratum.Rda") #save the variable importances

p13 <- plotnice(varimps_pool13_nostratum, color = "darkblue", title = "Point RF var. importance (5fold), pool 13")
# save as rf_avg_varimp_point_pool13_5foldcv.png
ggsave(plot = p13, 
       "rf_avg_varimp_point_pool13_5foldcv_landclass7nostratum.png",
       width = 7, height = 4.5)
save(cv_error_pool13_landclass7nostratum, file = "data/cv_error_pool13_landclass7nostratum.Rda")

cforest_errors_landclass7nostratum <- list(cv_error_all_landclass7nostratum, cv_error_pool4_landclass7nostratum, cv_error_pool8_landclass7nostratum, cv_error_pool13_landclass7nostratum)
save(cforest_errors_landclass7nostratum, file = "data/cforest_errors_landclass7nostratum.Rda")

#==================
# Load variable importances
load("data/varimps_all_nostratum.Rda")
varimps_all_nostratum$pool <- "all"
load("data/varimps_pool4_nostratum.Rda")
varimps_pool4_nostratum$pool <- "4"
load("data/varimps_pool8_nostratum.Rda")
varimps_pool8_nostratum$pool <- "8"
load("data/varimps_pool13_nostratum.Rda")
varimps_pool13_nostratum$pool <- "13"

# Bind together and reorder the factors
vi <- rbind(varimps_pool4_nostratum, varimps_pool8_nostratum, varimps_pool13_nostratum)
vi$predictorvars <- factor(vi$predictorvars, levels(vi$predictorvars)[c(1, 3, 8, 6, 2, 10, 11, 5, 9, 7, 4, 12)])
vi$pool <- as.factor(vi$pool)
vi$pool <- factor(vi$pool, levels(vi$pool)[c(2,3,1)])

# Make plot
d <- ggplot(data = vi, aes(x = predictorvars, y = imps, fill = pool))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name = "Pool", values = c("darkturquoise", "firebrick2", "royalblue4"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Permutation Importance (MDA)")+
  xlab("Variable")+
  ggtitle("Random forest variable importances by pool")+
  theme(text = element_text(size=20))

# Save plot
ggsave(plot = d, 
       "varimps_landclass7nostratum.png",
       width = 7.75, height = 5.5)

# plot importances for all together
k <- ggplot(data = varimps_all_nostratum, aes(x = predictorvars, y = imps))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Permutation Importance (MDA)")+
  xlab("Variable")+
  ggtitle("Random forest variable importances, all pools")+
  theme(text = element_text(size=20))

# Save plot
ggsave(plot = k, 
       "varimps_landclass7_all_nostratum.png",
       width = 7.75, height = 5.5)
