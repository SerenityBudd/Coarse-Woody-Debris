# Week 7 Sandbox
load("data/all_reduced.Rda")
source("libraries.R")
source("ownfunctions.R")
source("color_schemes.R")
# Summarize by polygon
# There are multiple strata per polygon so we'll exclude "stratum" as a column for that

poly <- all_reduced %>% group_by(uniq_id) %>% 
  filter(!is.na(snag)) %>%
                          summarize(propsnag = sum(snag)/n(),
                          AQUA_CODE = firstel(AQUA_CODE),
                          pool = firstel(pool),
                          Area = firstel(Area),
                          Perimeter = firstel(Perimeter),
                          max_depth = firstel(max_depth),
                          avg_depth = firstel(avg_depth),
                          tot_vol = firstel(tot_vol),
                          shoreline_density_index = firstel(shoreline_density_index),
                          pct_aqveg = firstel(pct_aqveg),
                          pct_terr = firstel(pct_terr),
                          pct_prm_wetf = firstel(pct_prm_wetf),
                          pct_terr_shore_wetf = firstel(pct_terr_shore_wetf),
                          NEAR_TERR_DIST.p = median(NEAR_TERR_DIST.p),
                          NEAR_FOREST_DIST.p = median(NEAR_FOREST_DIST.p),
                          pct_area_le100 = firstel(pct_area_le100)) %>%
  as.data.frame()
head(poly)
# impute NA values using the randomForest impute function
locate.nas(poly)
poly_imp <- rfImpute(x = poly[,4:ncol(poly)], y = poly$propsnag)
names(poly_imp)[1] <- "propsnag"

# PCA
poly.quant <- poly_imp %>% dplyr::select(-c(propsnag, pool))
locate.nas(poly.quant) # no NA's, so we're good to run PCA. 
a <- cor(poly.quant, use = "complete.obs")
    cp <- corrplot(a, type = "lower", method =  "color", diag = F)
    
pq <- poly.quant %>% scale() %>% as.data.frame()
pca <- pq %>% princomp()
pca$loadings

scores <- pca$scores %>% as.data.frame() %>% cbind(poly[,c("uniq_id", "propsnag", "AQUA_CODE", "pool")])
head(scores)
scores$pool <- factor(scores$pool, levels(scores$pool)[c(2,3,1)])


autoplot(prcomp(pq), data = scores, 
         colour = "pool",
         size = 2,
         loadings = T, 
         loadings.colour = "black",
         loadings.label = T,
         loadings.label.size = 3,
         loadings.label.colour = "black")+
  scale_color_manual(name = "Pool", values = c("red", "black", "blue"))+
  theme_bw()+
  #scale_color_manual(name = "Aquatic Habitat Type", values = AQUA_2010_colors)+
  ggtitle("PCA by polygon")+
  guides(pch = guide_legend(override.aes = list(size=12))) #legend size

autoplot(prcomp(pq), data = scores, colour = "AQUA_CODE", 
         size = 2, 
         loadings = T, 
         loadings.colour = "black", 
         loadings.label = T, 
         loadings.label.size = 3, 
         loadings.label.colour = "black")+
  scale_color_manual(name = "Aquatic Habitat Type", values = AQUA_2010_colors)+
  theme_bw()+
  ggtitle("PCA by polygon")+
  guides(pch = guide_legend(override.aes = list(size=12))) #legend size

save(poly, file = "data/poly.Rda")


