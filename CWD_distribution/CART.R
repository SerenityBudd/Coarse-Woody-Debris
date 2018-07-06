# CART
# Classification Tree with rpart
library(rpart)
load("data/new.ef.Rda")
load("data/pp.Rda")
load("ownfunctions.R")

head(pp,2)
pp$ll <- ifelse(is.na(pp$wdl_p_m2), "lentic", "lotic")
pp$ll <- factor(pp$ll)
table(pp$ll)
fittree <- rpart(snagyn ~ dist_landcover + Area + Perimeter + avg_depth + shoreline_density_index + pct_prm_lotic + pct_prm_lentic + pct_aqveg + pct_opwat + pct_terr + pct_aq + pct_prm_wetf + pct_terr_shore_wetf + wdl_p_m2 + pct_terr_shore_rev + pct_prm_rev + sinuosity + pct_area_le50 + pct_area_le100 + depth + current + stratum + ll,
                 method = "class", 
                 data = pp, cp = 0) #cp default is zero, setting it higher just cuts off the tree earlier. 
# we don't want any prior probabilities because we have no reason to believe that there are prior probabilities of CWD occurring or not
# we don't need a loss matrix because we don't particularly care which way our error goes. One type of misclassification error isn't better or worse than the other. 


printcp(fittree)
plotcp(fittree, col = "red")
#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
#from this plot, it looks like a good value to use is cp = 0.005. 
fittree2 <- prune(fittree, cp = 0.005)

plot(fittree2, uniform=TRUE, 
     main="Classification Tree for Coarse Woody Debris")

text(fittree2, use.n=TRUE, all=TRUE, cex=.8)
