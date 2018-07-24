
#========================================
# cforest tree on point level for larger sample size
#========================================
load("data/all_reduced.Rda")
all_reduced <- all_reduced %>% dplyr::select(-c(barcode, uniq_id, Area, pct_prm_lotic, pct_prm_lentic, num_lotic_outl, num_lentic_outl, econ, pct_terr_shore_wetf, sinuosity, NEAR_TERR_CLASS_31_N.p, NEAR_FOREST_CLASS_31_N.p, year.p, stageht.p, stratum_name, snagyn, pct_area_le100, depth.p, current.p, substrt.p, trib.p)) %>% na.omit()
all <- all_reduced
#=======================
# Points, all pools 
#=======================
all <- splitup(all, 5) 

vars <- colnames(all)[c(1, 3:18)]
importances <- data.frame(
  vars
)

for(i in 1:5){
  pointTemp <- all[all$grp == i, 1:18]
  
  tree <- partykit::cforest(snag~., 
                            data = pointTemp,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  vi.part <- partykit::varimp(tree)
  importances[,i] <- vi.part
  print(paste("Run", i, "complete"))
}

impsums <- rowSums(importances)
imps <- impsums/5
varimps <- cbind(vars, imps) %>% as.data.frame %>% mutate(imps = as.numeric(as.character(imps))) %>% mutate(vars = reorder(vars, -imps))

plotnice(varimps, color = "olivedrab4", pool = "all pools, points")


#=============================
# pool 4

vars <- colnames(all)[c(1, 3, 5:18)]
importances <- data.frame(
  vars
)

points_4 <- splitup(all[all$pool == 4,], 5)

for(i in 1:5){
  pointTemp <- points_4[points_4$grp == i, c(1:3, 5:19)]
  
  tree <- partykit::cforest(snag~., 
                            data = pointTemp,
                            ntree = 1000, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  vi.part <- partykit::varimp(tree)
  importances[,i] <- vi.part
  print(paste("Run", i, "complete"))
}

impsums <- rowSums(importances)
imps <- impsums/5
varimps <- cbind(vars, imps) %>% as.data.frame %>% mutate(imps = as.numeric(as.character(imps))) %>% mutate(vars = reorder(vars, -imps))

plotnice(varimps, color = "cadetblue4", pool = "pool 4, points")

#=============================
# pool 8

points_8 <- splitup(all[all$pool == 8,], 5)

for(i in 1:5){
  pointTemp <- points_8[points_8$grp == i, c(1:3, 5:19)]
  
  tree <- partykit::cforest(snag~., 
                            data = pointTemp,
                            ntree = 1000, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  vi.part <- partykit::varimp(tree)
  importances[,i] <- vi.part
  print(paste("Run", i, "complete"))
}

impsums <- rowSums(importances)
imps <- impsums/5
varimps <- cbind(vars, imps) %>% as.data.frame %>% mutate(imps = as.numeric(as.character(imps))) %>% mutate(vars = reorder(vars, -imps))

plotnice(varimps, color = "brown", pool = "pool 8, points")

#=============================
# pool 13

points_13 <- splitup(all[all$pool == 13,], 5)

for(i in 1:5){
  pointTemp <- points_13[points_13$grp == i, c(1:3, 5:19)]
  
  tree <- partykit::cforest(snag~., 
                            data = pointTemp,
                            ntree = 1000, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  vi.part <- partykit::varimp(tree)
  importances[,i] <- vi.part
  print(paste("Run", i, "complete"))
}

impsums <- rowSums(importances)
imps <- impsums/5
varimps <- cbind(vars, imps) %>% as.data.frame %>% mutate(imps = as.numeric(as.character(imps))) %>% mutate(vars = reorder(vars, -imps))

plotnice(varimps, color = "darkblue", pool = "pool 13, points")
