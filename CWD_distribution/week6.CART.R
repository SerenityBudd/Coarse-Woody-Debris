#CART modeling
##########################################################
# CART
# Classification Tree with rpart
load("data/sites_aa_5m.Rda")
source("ownfunctions.R")
source("libraries.R")

# Exclude points where snag is NA and fixed sites. 
# Pool 4
p4_5 <- sites_aa_5m %>% filter(pool == 4, !is.na(snag), sitetype != 2)
# Pool 8
p8_5 <- sites_aa_5m %>% filter(pool == 8, !is.na(snag), sitetype != 2)
# Pool 13
p13_5 <- sites_aa_5m %>% filter(pool == 13, !is.na(snag), sitetype != 2)
# All
sites_aa_5m <- sites_aa_5m %>% filter(!is.na(snag), sitetype != 2)
#using stratum for predictive purposes
#using riprap instead of the revetment values.
formula <- snag ~ stratum + pool + AQUA_CODE + Area + avg_depth + shoreline_density_index + pct_prm_lotic + pct_prm_lentic + wdl_p_m2 + pct_terr + pct_prm_wetf + sinuosity + NEAR_TERR_DIST + NEAR_TERR_CLASS_31 + NEAR_FOREST_DIST + NEAR_FOREST_CLASS_31 + depth.p + current.p + riprap.p + substrt.p + trib.p + wingdyke.p + pct_area_le100 #+ year.p

tree.all <- rpart(formula,
                  method = "class", 
                  data = sites_aa_5m, cp = 0)

printcp(tree.all)
plotcp(tree.all, col = "red", las = 2)

#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
tree.all.2 <- prune(tree.all, cp = 0.002)
printcp(tree.all.2)

sites_aa_5m$CARTpreds.all <- predict(tree.all.2, type = "class")
cm.trall.teall <- with(sites_aa_5m, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.teall <- cm.trall.teall[1,2] + cm.trall.teall[2,1]
er.trall.teall
#test on pool 4
p4_5$CARTpreds.all <- predict(tree.all.2, newdata = p4_5, type = "class")
cm.trall.te4 <- with(p4_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te4 <- cm.trall.te4[1,2] + cm.trall.te4[2,1]
er.trall.te4
#test on pool 8
p8_5$CARTpreds.all <- predict(tree.all.2, newdata = p8_5, type = "class")
cm.trall.te8 <- with(p8_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te8 <- cm.trall.te8[1,2] + cm.trall.te8[2,1]
er.trall.te8
#test on pool 13
p13_5$CARTpreds.all <- predict(tree.all.2, newdata = p13_5, type = "class")
cm.trall.te13 <- with(p13_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te13 <- cm.trall.te13[1,2] + cm.trall.te13[2,1]
er.trall.te13

rpart.plot(x = tree.all.2, type = 5, cex = 0.5, main = "Tree for Pools 4, 8, and 13")

### pool4 only 
tree.p4 <- rpart(formula,
                 method = "class", 
                 data = p4_5, cp = 0)

printcp(tree.p4)
plotcp(tree.p4, col = "red", las = 2)
tree.p4.2 <- prune(tree.p4, cp = 0.005)
printcp(tree.p4.2)

#test on pool 4
p4_5$CARTpreds.p4 <- predict(tree.p4.2, type = "class")
cm.tr4.te4 <- with(p4_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te4 <- cm.tr4.te4[1,2] + cm.tr4.te4[2,1]
er.tr4.te4
#test on pool 8
p8_5$CARTpreds.p4 <- predict(tree.p4.2, newdata = p8_5, type = "class")
cm.tr4.te8 <- with(p8_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te8 <- cm.tr4.te8[1,2] + cm.tr4.te8[2,1]
er.tr4.te8
#test on pool 13
p13_5$CARTpreds.p4 <- predict(tree.p4.2, newdata = p13_5, type = "class")
cm.tr4.te13 <- with(p13_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te13 <- cm.tr4.te13[1,2] + cm.tr4.te13[2,1]
er.tr4.te13

rpart.plot(x = tree.p4.2, type = 5, cex = 0.7, main = "Tree for Pool 4")

### pool8 only 
tree.p8 <- rpart(formula,
                 method = "class", 
                 data = p8_5, cp = 0)

printcp(tree.p8)
plotcp(tree.p8, col = "red", las = 2)
tree.p8.2 <- prune(tree.p8, cp = 0.006)
printcp(tree.p8.2)

#test on pool 8
p8_5$CARTpreds.p8 <- predict(tree.p8.2, type = "class")
cm.tr8.te8 <- with(p8_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te8 <- cm.tr8.te8[1,2] + cm.tr8.te8[2,1]
er.tr8.te8
#test on pool 4
p4_5$CARTpreds.p8 <- predict(tree.p8.2, newdata = p4_5, type = "class")
cm.tr8.te4 <- with(p4_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te4 <- cm.tr8.te4[1,2] + cm.tr8.te4[2,1]
er.tr8.te4
#test on pool 13
p13_5$CARTpreds.p8 <- predict(tree.p8.2, newdata = p13_5, type = "class")
cm.tr8.te13 <- with(p13_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te13 <- cm.tr8.te13[1,2] + cm.tr8.te13[2,1]
er.tr8.te13

rpart.plot(x = tree.p8.2, type = 5, cex = 0.7, main = "Tree for Pool 8")

### pool13 only 
tree.p13 <- rpart(formula,
                  method = "class", 
                  data = p13_5, cp = 0)

printcp(tree.p13)
plotcp(tree.p13, col = "red", las = 2)
tree.p13.2 <- prune(tree.p13, cp = 0.008)
printcp(tree.p13.2)

#test on pool 13
p13_5$CARTpreds.p13 <- predict(tree.p13.2, type = "class")
cm.tr13.te13 <- with(p13_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te13 <- cm.tr13.te13[1,2] + cm.tr13.te13[2,1]
er.tr13.te13
#test on pool 4
p4_5$CARTpreds.p13 <- predict(tree.p13.2, newdata = p4_5, type = "class")
cm.tr13.te4 <- with(p4_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te4 <- cm.tr13.te4[1,2] + cm.tr13.te4[2,1]
er.tr13.te4
#test on pool 8
p8_5$CARTpreds.p13 <- predict(tree.p13.2, newdata = p8_5, type = "class")
cm.tr13.te8 <- with(p8_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te8 <- cm.tr13.te8[1,2] + cm.tr13.te8[2,1]
er.tr13.te8

rpart.plot(x = tree.p13.2, type = 5, cex = 0.5, main = "Tree for Pool 13")