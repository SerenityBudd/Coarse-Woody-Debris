# Week 7 Sandbox
load("data/all_reduced.Rda")
load("data/poly.Rda")
# impute NA values using the randomForest impute function
locate.nas(poly)
poly_imp <- rfImpute(x = poly[,4:ncol(poly)], y = poly$propsnag)
names(poly_imp)[1] <- "propsnag"

# PCA
poly.quant <- poly_imp %>% dplyr::select(-c(propsnag, pool, AQUA_CODE))
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



