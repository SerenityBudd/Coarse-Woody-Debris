load("data/poly.Rda")
source("rfPerformanceStats.R")
# impute NA values using the randomForest impute function
locate.nas(poly)
poly_imp <- rfImpute(x = poly[,4:ncol(poly)], y = poly$propsnag)
names(poly_imp)[1] <- "propsnag"
poly_imp <- cbind(poly[,c("uniq_id", "AQUA_CODE")], poly_imp) #add back the categoricals
locate.nas(poly_imp)

# split propsnag into snag presence/absence
poly_imp$pa <- NULL
poly_imp$pa[poly_imp$propsnag < .5] <- "absence"
poly_imp$pa[poly_imp$propsnag >= .5] <- "presence"
poly_imp$pa <- as.factor(poly_imp$pa)
# now we have very unbalanced classes.
poly_imp <- poly_imp %>% dplyr::select(-c("propsnag", "uniq_id"))

a <- sample(nrow(poly_imp[poly_imp$pa == "absence",]), 200, replace = F)
p <- sample(nrow(poly_imp[poly_imp$pa == "presence",]), 152, replace = F)
train <- poly_imp[c(a,p),]
test <- poly_imp[-c(a,p),]

# how many samples are in the minority class?
nmin <- nrow(train[train$pa == "absence",])
nmin # use this for undersampling
nmaj <- nrow(train[train$pa == "presence",])
nmaj # use this for oversampling

#undersampling
oversamp.errors <- vector(mode = "list", length = 50)
cutoff_abs <- seq(from = 0.05, to = 0.95, by = 0.01)
cutoff_pres <- seq(from = 0.95, to = 0.05, by = -0.01)
for(j in 1:5){
set.seed(j)
overall_error_train <- rep(NA, length(cutoff_abs))
pres_error_train <- rep(NA, length(cutoff_abs))
abs_error_train <- rep(NA, length(cutoff_abs))
pres_error_test <- rep(NA, length(cutoff_abs))
abs_error_test <- rep(NA, length(cutoff_abs))
overall_error_test <- rep(NA, length(cutoff_abs))
for(i in 1:length(cutoff_abs)){
  tree <- randomForest(pa~.,
                       data = train,
                       strata = "pa",
                       sampsize = c(nmin, nmin),
                       ntree = 4000,
                       cutoff = c(cutoff_abs[i], cutoff_pres[i]))
  #predict on training data
  pred <- predict(tree, newdata = train, type = "class", cutoff = c(cutoff_abs[i], cutoff_pres[i]))
  overall_error_train[i] <- mean(pred!=train$pa, na.rm = T)
  pres_error_train[i] <- mean(pred[train$pa == "presence"] != train$pa[train$pa == "presence"], na.rm = T)
  abs_error_train[i] <- mean(pred[train$pa == "absence"] != train$pa[train$pa == "absence"], na.rm = T)
  
  # predict on the test data
  pred <- predict(tree, newdata = test, type = "class", cutoff = c(cutoff_abs[i], cutoff_pres[i]))
  overall_error_test[i] <- mean(pred!=test$pa, na.rm = T)
  pres_error_test[i] <- mean(pred[test$pa == "presence"] != test$pa[test$pa == "presence"], na.rm = T)
  abs_error_test[i] <- mean(pred[test$pa == "absence"] != test$pa[test$pa == "absence"], na.rm = T)
}
type <- c(rep("overall train", 19), rep("overall test", 19), rep("pres train", 19), rep("abs train", 19), rep("pres test", 19), rep("abs test", 19))
otr <- cbind(cutoff_abs, overall_error_train)
ote <- cbind(cutoff_abs, overall_error_test)
ptr <- cbind(cutoff_abs, pres_error_train)
atr <- cbind(cutoff_abs, abs_error_train)
pte <- cbind(cutoff_abs, pres_error_test)
ate <- cbind(cutoff_abs, abs_error_test)

oversamp.errors[[j]] <- rbind(otr, ote, ptr, atr, pte, ate) %>% as.data.frame() %>% mutate(type = type) %>% rename(error = overall_error_train)
print(paste("Run", j, "complete"))
}

df <- oversamp.errors[[1]][,2]
for(i in 2:length(oversamp.errors)){
  df <- cbind(df, oversamp.errors[[i]][,2])
}
avgerrors <- rowSums(df)/ncol(df)
errors <- cbind(oversamp.errors[[1]][,1], avgerrors, oversamp.errors[[1]][,3]) %>% as.data.frame() %>% rename(cutoff_abs = V1, error = avgerrors, type = V3)

errors$cutoff_abs <- as.numeric(as.character(errors$cutoff_abs))
errors$error <- as.numeric(as.character(errors$error))

errors %>% filter(type %in% c("overall train", "abs train", "pres train")) %>%
ggplot(aes(x = cutoff_abs, y = error, color = type))+
  geom_line(size = 1.2) +
  theme_bw() +
  ggtitle("Training Data Errors")

errors %>% filter(type %in% c("overall test", "abs test", "pres test")) %>%
  ggplot(aes(x = cutoff_abs, y = error, color = type))+
  #geom_point()+
  geom_line(size = 1.2) +
  theme_bw()+
  ggtitle("Test Data Errors for Oversampled Data")

nrow(train[train$pa == "absence",])/nrow(train)
nrow(test[test$pa == "absence",])/nrow(test)
