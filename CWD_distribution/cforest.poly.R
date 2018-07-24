# cforest conditional random forest: supposed to be more accurate and unbiased, according to Strobl et al. 2007
library(party)
library(partykit)
?partykit::cforest
#===========================
# Random forest *regression* tree to predict `propsnag` on polygons
#===========================
load("data/poly.Rda")
source("libraries.R")
source("ownfunctions.R")
locate.nas(poly)
poly <- na.omit(poly) #for now, we'll just drop the rows that have NA's in them. But I don't understand why the depth information is missing, and this may significantly affect the models. Can we fill in the missing data?

poly <- poly %>% dplyr::select(-c(uniq_id, n))

tree_reg <- cforest(formula = propsnag~., #using conditional random forests for less bias
                    data = poly,
                    ntree = 1000,
                    trace = T, #show progess bar
                    mtry = floor(ncol(poly)/3),
                    perturb = list(replace = F, fraction = 0.632)) #following Strobl et al.. 2007

# according to this paper (https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-119), the variable importances may be biased when there is class imbalance. The party package documentation suggests that there is a function varimpAUC that will calculate AUC-based permutation variable importances, but it doesn't work properly for objects created with partykit::cforest, because the objects are structured differently than party::cforest objects. Could recreate this AUC-based metric, but for the purposes of this paper that's probably not time-efficient.

plotimportance <- function(tree_reg, color, title){
  vi <- varimp(tree_reg) %>% as.data.frame()
  vi$var <- row.names(vi)
  names(vi) <- c("imp", "var")
  vi <- vi %>% mutate(var = factor(var)) %>% mutate(var = reorder(var, -imp))
 
  # Make a pretty plot of the importances
ggplot(data = vi, aes(x = var, y = imp))+
  geom_col(fill = color)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())+
  ylab("Relative Importance")+
  ggtitle(title)+
  theme(text = element_text(size = 20))
}
plotimportance(tree_reg, color = "olivedrab4", title = "Cond. RF variable importance for all pools")

#==============================
# How transferrable is this between pools?
#==============================
poly_4 <- poly[poly$pool == 4,] #pool 4 data

poly_8 <- poly[poly$pool == 8,] #pool 8 data

poly_13 <- poly[poly$pool == 13,] #pool 13 data

# Make a regression tree for pool 8
tree8 <- cforest(formula = propsnag~., #conditional random forest
                    data = poly_8, #pool 8 only
                    ntree = 1000,
                    trace = T, #show progess bar
                    mtry = floor(ncol(poly_8)/3),
                    perturb = list(replace = F, 
                                   fraction = 0.632)) #following Strobl et al.. 2007

plotimportance(tree8, color = "brown", title = "Cond. RF variable importance for pool 8")

# Make a regression tree for pool 4
tree4 <- cforest(propsnag ~., 
                      data = poly_4, 
                      ntree = 1000, 
                      trace = T,
                      mtry = floor(ncol(poly_4)/3),
                      perturb = list(replace = F, 
                                     fraction = 0.632)) #following Strobl et al.. 2007)
plotimportance(tree4, color = "cadetblue4", title = "Cond. RF variable importance for pool 4")

# Make a regression tree for pool 13
tree13 <- cforest(propsnag ~., 
                       data = poly_13, 
                       ntree = 1000, 
                       trace = T,
                       mtry = floor(ncol(poly_13)/3),
                       perturb = list(replace = F,
                                      fraction = 0.632))
plotimportance(tree13, color = "darkblue", title = "Cond. RF variable importance for pool 13")

#================================
# compute average variable importance over 5 folds
#================================

#============================
# ALL POLYGONS
#============================
splitup <- function(poly, k){
set.seed(1)
inds <- sample(rep(1:k, length = nrow(poly)))
poly$grp <- inds
return(poly)
}

poly <- splitup(poly, 5)

vars <- colnames(poly)[2:16]
importances <- data.frame(
  vars
  )

for(i in 1:5){
  polyTemp <- poly[poly$grp != i, 1:16]
  
  tree <- partykit::cforest(propsnag~., 
                            data = polyTemp,
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

#nicer variable importance plot
plotnice <- function(varimps, color, pool){
ggplot(data = varimps, aes(x = vars, y = imps))+
  geom_col(fill = color)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Rel. importance")+
  ggtitle(paste0("Polygon RF avg var imp for ", pool))+
  theme(text = element_text(size=20))
}
plotnice(varimps, color = "olivedrab", pool = "all pools")

