# CART
# Classification Tree with rpart
library(rpart)
load("data/new.ef.Rda")
load("data/pp.Rda")
source("ownfunctions.R")
source("libraries.R")

new.ef$ll <- ifelse(is.na(new.ef$wdl_p_m2), "lentic", "lotic")
new.ef$ll <- factor(new.ef$ll)
table(new.ef$ll)

#none of the variables should have a negative value
for(i in 1:ncol(new.ef)){
  new.ef[,i][new.ef[,i] < 0] <- NA
}

#pct_prm_rev and pct_terr_shore_rev should not have any values greater than 100
new.ef$pct_prm_rev[new.ef$pct_prm_rev > 100] <- NA
new.ef$pct_terr_shore_rev[new.ef$pct_terr_shore_rev > 100] <- NA
new.ef$pct_opwat[new.ef$pct_opwat > 100] <- NA


fittree <- rpart(snag ~ stratum + Area + avg_depth + avg_fetch + shoreline_density_index + pct_prm_lotic + pct_prm_lentic + pct_opwat + pct_terr + pct_aq + pct_prm_wetf + pct_terr_shore_wetf + wdl_p_m2 + num_wd + len_wd + pct_terr_shore_rev + pct_prm_rev + sinuosity + pct_area_le100 + pct_area_le50 + landcover_lumped + depth + current + riprap + do + econ,
                 method = "class", 
                 data = new.ef, cp = 0) #cp default is zero, setting it higher just cuts off the tree earlier. 
# we don't want any prior probabilities because we have no reason to believe that there are prior probabilities of CWD occurring or not
# we don't need a loss matrix because we don't particularly care which way our error goes. One type of misclassification error isn't better or worse than the other. 
#I don't know what the parameter "split" is.

printcp(fittree)
plotcp(fittree, col = "red", las = 2)

#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
#from this plot, it looks like a good value to use is cp = 0.005. 
fittree2 <- prune(fittree, cp = 0.021)
printcp(fittree2)

plot(fittree2, uniform=TRUE, 
     main="Classification Tree for Coarse Woody Debris")

text(fittree2, 
     use.n=TRUE, 
     all=TRUE, 
     cex=.8, 
     pretty = F) #pretty = F includes full text of factor levels on the nodes

rpart.plot(x = fittree2, type = 5, cex = 0.6)


