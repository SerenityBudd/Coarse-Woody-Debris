# Week 5 sandbox, 7/2/18 - 7/8/18
source("libraries.R")
source("color_schemes.R")
load("data/new.ef.Rda")
source("locate.nas.R")
source("firstel.R")

#Separate out the quantitative predictors and make a scatterplot matrix
pp <- new.ef[,c("dist_landcover", "Area", "Perimeter", #1-3
                "avg_depth", "shoreline_density_index", "pct_prm_lotic", #4-6
                "pct_prm_lentic", "pct_aqveg", "pct_opwat", #5-9
                "pct_terr", "pct_aq", "pct_prm_wetf", #10-13
                "pct_terr_shore_wetf", "wdl_p_m2", "pct_terr_shore_rev", #14-16
                "pct_prm_rev", "sinuosity", "pct_area_le50", #17-19
                "pct_area_le100", "depth", "current", #20-22
                "snag", "snagyn", "stratum")] #23-25

    #none of these should have negative values
    #change negative values to NA's
    for(i in 1:ncol(pp)){
      pp[,i][pp[,i] < 0] <- NA
    }
    
#example scatterplot matrix
    pairs(pp[,1:7])
    #can run Spearman correlation with NA's, but a lot of correlations still fail to calculate.
    c <- cor(pp[,1:22], method = "spearman", use = "complete.obs")
    cp <- corrplot(c, method = "circle", type = "upper", diag = F)
    
#prepare to run univariate logistic regression on a few of these variables
  vars <- colnames(pp)[!colnames(pp) %in% c("snag", "snagyn", "stratum")]
  names(vars) <- c("Dist. to Nearest Land", "Area of Polygon", "Perimeter of Polygon",
                   "Avg. Depth of Polygon", "Polygon Shoreline Density Index", "% Poly. Perim. adj. to Lotic Areas",
                   "% Poly. Perim. adj. to Lentic Areas", "% Aquatic Vegetation in Polygon", "% Open Water in Polygon",
                   "% Poly. Perim. adj. to Terrestrial Areas", "% Poly Perim. adj. to Aquatic Areas", "% Poly. Perim. adj. to Wet Forest",
                   "% Terrestrial Shoreline adj. to Wet Forest", "Wing Dam Length per m^2 area", "% Terrestrial Shoreline w/ Revetment",
                   "% Poly. Perim. w/ Revetment", "Polygon Sinuosity", "% Poly. Area <= 50cm Depth",
                   "% Poly. Area <= 100cm Depth", "Depth", "Current")

source("CWD_distribution/univ.logit.regression.script.R")
outputlist <- vector("list", length(vars)) 

for(i in 1:length(vars)){
  a <- makelogit(df_source = pp, 
            var_of_interest = vars[[i]], 
            cwdfactor = "snagyn", 
            cwd = "snag", 
            varname = names(vars)[i])
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
      
      
#############################################################################
      # Models with interactions with stratum types
table(pp$stratum)
      # There are only 2 points in the tailwater zone and 16 in the impounded open water zone, let's remove these for the purposes of this analysis. 
    pp <- droplevels(pp[!pp$stratum %in% c("TWZ", "IMP-O"),])
    table(pp$stratum)
      # These sample sizes seem much more reasonable
      
    source("CWD_distribution/interact.univ.logit.regression.script.R")
    outputlist.interact <- vector("list", length(vars))
    
    for(i in 1:length(vars)){
        a <- makelogit.interact(df_source = pp, 
                       var_of_interest = vars[[i]], 
                       cwdfactor = "snagyn", 
                       cwd = "snag", 
                       varname = names(vars)[i],
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
  
### PCA on the variables (or some other ordination method)
#MDS for rotating, visualizing axes better
#PCA is a special case of MDS
#MDS simpler for plotting 
  
  #Can't do PCA with NA values: Let's investigate where we have NA's.
  navec <- rep (NA, ncol(pp))
  locate.nas(pp)
  #We have NA's in the following columns:
  # `avg_depth` : don't know why. No zeroes
  # `pct_prm_lotic`: metric not calc. for channel border polygons.
  # `wdl_p_m2`: calc. for lotic only
  # `pct_terr_shore_rev`: calc. for lotic only
  # `pct_prm_rev`: calc. for lotic only
  # `sinuosity`: calc. for lotic only
  # `pct_aqveg`: no idea why we have 6 NA's here. 
  
  #for now, let's leave out those variables.
  
ppa <- pp %>% select(-c("avg_depth", "pct_aqveg", "pct_prm_lotic", "wdl_p_m2", "pct_terr_shore_rev", "pct_prm_rev", "sinuosity"))

pairs(ppa[,1:12])
c <- cor(ppa[,1:12])
corrplot(c, method = "color", type = "lower", diag = F)
#let's also leave out pct_aq (since it's the inverse of pct_terr). 
ppa <- ppa %>% select(-c("pct_aq"))
c2 <- cor(ppa[,1:11])
corrplot(c2, method = "color", type = "lower", diag = F)
#this is a bit better. Still see a few highly correlated variables. 
  
# Run PCA
ppa_pca <- princomp(ppa[,1:11], cor = T)
  summary(ppa_pca)
  ppa_pca$loadings
  
  # How many PC's should we keep?
  (ppa_pca$sdev)^2  #shows eigenvalues. This suggests we should keep 4 principal components.

  #scree plot
  plot(1:(length(ppa_pca$sdev)),  
       (ppa_pca$sdev)^2, type='b', 
       main="Scree Plot",
       xlab="Number of Components", 
       ylab="Eigenvalue Size")
      #pretty inconclusive scree plot.
      
  # Plotting the PC scores for the sample data in the space of the first two principal components:
  scores <- as.data.frame(ppa_pca$scores)
  scores$snagyn <- ppa$snagyn
  scores$stratum <- ppa$stratum
  
  #Score plot of components 2 and 3, showing separation by aquatic stratum
  ggplot(data = scores, aes(x = Comp.2, 
                            y = Comp.3, 
                            color = stratum))+
    geom_point()+
    scale_color_manual(name = "Stratum", 
                       values = strataColors_codes)+
    ggtitle("PCA Score Plot of Sampling Sites")
  
  #Score plot of components 2 and 3, showing separation by CWD presence
  ggplot(data = scores, aes(x = Comp.1, 
                            y = Comp.3, 
                            color = snagyn))+
    geom_point()+
    scale_color_manual(name = "CWD", 
                       values = c("black", "red"))+
    ggtitle("PCA Score Plot of Sampling Sites")


# What if we run PCA separately for each aquatic habitat type?
  pca_scb <- princomp(ppa[ppa$stratum == "SCB",1:11], cor = T)
  summary(pca_scb)
  pca_scb$loadings
  
  pca_bwcs <- princomp(ppa[ppa$stratum == "BWC-S",1:11], cor = T)
  summary(pca_bwcs)
  pca_bwcs$loadings
  
  pca_imps <- princomp(ppa[ppa$stratum == "IMP-S",1:11], cor = T)
  summary(pca_imps)
  pca_imps$loadings
    pca_imps_scores <- as.data.frame(pca_imps$scores)
    pca_imps_scores$snagyn <- ppa[ppa$stratum == "IMP-S",]$snagyn
  #this one looks a little more promising
    #Score plot of components 2 and 3, showing separation by CWD presence
    ggplot(data = pca_imps_scores, aes(x = Comp.2, 
                              y = Comp.3, 
                              color = snagyn))+
      geom_point()+
      scale_color_manual(name = "CWD", 
                         values = c("black", "red"))+
      ggtitle("PCA Score Plot of Sampling Sites")

    
###################################################################   
# Now let's do some analyses breaking the data down by polygon instead of by sampling site.
model1 <- lm(propcwd~pct_terr, data = new.ef)
source("firstel.R")
    bypoly <- as.data.frame(new.ef %>% 
                              group_by(OBJECTID) %>%
                              summarize_all(firstel))
    