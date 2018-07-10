source("libraries.R")
source("ownfunctions.R")
load("data/sites_aa.Rda")
load("data/sites_aa_5m.Rda")
load("data/p8_5.Rda")
#Run the week 5 code with the new data (5m buffer)
q <- p8_5 %>% select(barcode, snag, snagyn, stratum, Area, Perimeter, avg_depth, tot_vol, avg_fetch, shoreline_density_index, pct_terr, pct_prm_wetf, pct_terr_shore_wetf, pct_terr_shore_rev, pct_prm_rev, sinuosity, NEAR_TERR_DIST, NEAR_FOREST_DIST, depth.p, pct_area_le100) %>% filter(!is.na(snag))
c <- p8_5 %>% select(barcode, snag, snagyn, stratum, stratum_name, AQUA_CODE, NEAR_TERR_CLASS_31_N, NEAR_TERR_CLASS_15_N, NEAR_TERR_CLASS_7_N, NEAR_TERR_HEIGHT_N, NEAR_FOREST_CLASS_31_N, NEAR_FOREST_CLASS_15_N, NEAR_FOREST_CLASS_7_N, NEAR_FOREST_HEIGHT_N, gear.p, riprap.p, substrt.p, wingdyke.p) %>% filter(!is.na(snag))

#prepare to run univariate logistic regression on a few of these variables
vars <- colnames(q)[!colnames(q) %in% c("barcode", "snag", "snagyn", "stratum")]

source("CWD_distribution/univ.logit.regression.script.R")
outputlist <- vector("list", length(vars)) 

for(i in 1:length(vars)){
  a <- makelogit(df_source = q, 
                 var_of_interest = vars[[i]], 
                 cwdfactor = "snagyn", 
                 cwd = "snag", 
                 varname = vars[i])
  outputlist[[i]] <- a
}
#name the elements of the output list
names(outputlist) <- vars

#Let's get these outputs sorted by type
#initialize lists for the output objects
boxplots <- vector("list", length(vars))
densityplots <- vector("list", length(vars))
modelsummaries <- vector("list", length(vars))
predictions <- vector("list", length(vars))
logitplots <- vector("list", length(vars))

#fill the lists
for(i in 1:length(vars)){
  boxplots[[i]] <- outputlist[[i]]$boxplot
  densityplots[[i]] <- outputlist[[i]]$density_plot
  modelsummaries[[i]] <- outputlist[[i]]$model_summary
  predictions[[i]] <- outputlist[[i]]$predictions
  logitplots[[i]] <- outputlist[[i]]$logit_plot
}

# data frame for model coefficients and p values
coef.pval <- data.frame(
  index = 1:length(vars),
  var = vars,
  coef = rep(NA, length(vars)),
  pval = rep(NA, length(vars))
)
# fill data frame with coefficients and p values
for(i in 1:length(vars)){
  coef.pval$pval[i] <- outputlist[[i]]$p_value
  coef.pval$coef[i] <- coef(outputlist[[i]]$model)[2]
}

# remove the text from the p-values, convert to numeric, round to 6 sig figs
coef.pval$pval <- gsub("p = ", "", coef.pval$pval) %>% 
  as.numeric() %>% 
  signif(digits = 6)

# Add significance codes
coef.pval$sig001 <- ifelse(coef.pval$pval <= 0.001, "**", NA)
coef.pval$sig01 <- ifelse(coef.pval$pval <= 0.01, "*", NA)
coef.pval$sig05 <- ifelse(coef.pval$pval <= 0.05, ".", NA)
head(coef.pval)

# Let's examine the logit plots for the variables that were significant at a 0.05 level.
indices <- coef.pval$index[!is.na(coef.pval$sig05)]
densityplots[indices]
logitplots[indices]


# Examine the ones that were not significant at alpha = 0.05
densityplots[-indices]
logitplots[-indices]

# Save all logit plots
for(i in 1:length(logitplots)){
  ggsave(filename = paste0("figure_images/", "logit.", names(outputlist)[i], ".jpg"), 
         plot = logitplots[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}

# Save all density plots
for(i in 1:length(densityplots)){
  ggsave(filename = paste0("figure_images/", "density.", names(outputlist)[i], ".jpg"), 
         plot = densityplots[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}


#############################################################################
# Models with interactions with stratum types
table(droplevels(q$stratum))
# There are only 2 points in the tailwater zone and 16 in the impounded open water zone, let's remove these for the purposes of this analysis. 
q <- droplevels(q[!q$stratum %in% c("TWZ", "IMP-O"),])
table(q$stratum)
# These sample sizes seem much more reasonable

source("CWD_distribution/interact.univ.logit.regression.script.R")
outputlist.interact <- vector("list", length(vars))

for(i in 1:length(vars)){
  a <- makelogit.interact(df_source = q, 
                          var_of_interest = vars[[i]], 
                          cwdfactor = "snagyn", 
                          cwd = "snag", 
                          varname = vars[i],
                          stratum = "stratum")
  outputlist.interact[[i]] <- a
}

#name the elements of the output list
names(outputlist.interact) <- vars

#Let's get these outputs sorted by type
#initialize lists for the output objects
densityplots.interact <- vector("list", length(vars))
modelsummaries.interact <- vector("list", length(vars))
predictions.interact <- vector("list", length(vars))
logitplots.interact <- vector("list", length(vars))

#fill the lists
for(i in 1:length(vars)){
  densityplots.interact[[i]] <- 
    outputlist.interact[[i]]$densityplot_stratum
  modelsummaries.interact[[i]] <- outputlist.interact[[i]]$model_summary
  predictions.interact[[i]] <- outputlist.interact[[i]]$predictions.interact
  logitplots.interact[[i]] <- outputlist.interact[[i]]$logit_plot
}


#Save the logit plots
for(i in 1:length(logitplots.interact)){
  ggsave(filename = paste0("figure_images/", "logitinteract.", names(outputlist.interact)[i], ".jpg"), 
         plot = logitplots.interact[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}
#Save the density plots
for(i in 1:length(densityplots.interact)){
  ggsave(filename = paste0("figure_images/", "density_stratum.", names(outputlist.interact)[i], ".jpg"), 
         plot = densityplots.interact[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}

#What if I want to make the lines only go as far as they should based on the standard error clouds?


#Random forest modeling in `caret` package: black box. Easy to learn, hard to understand. 
library(caret)
