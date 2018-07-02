# Week 5 sandbox, 7/2/18 - 7/8/18
source("libraries.R")
source("color_schemes.R")
load("data/new.ef.Rda")

#Separate out the quantitative predictors and make a scatterplot matrix
pp <- new.ef[,c("Area", "Perimeter", "sinuosity", "avg_fetch", "max_depth", "avg_depth", "tot_vol", "bath_pct", "area_gt100", "len_wetf", "pct_prm_wetf", "pct_terr_shore_wetf", "len_prm_lotic", "pct_prm_lotic", "num_lotic_outl", "len_prm_lentic", "pct_prm_lentic", "num_lentic_outl", "pct_aqveg", "pct_opwat", "len_terr", "pct_terr", "pct_aq", "len_wd", "wdl_p_m2", "num_wd", "len_revln", "pct_prm_rev", "pct_terr_shore_rev", "snag", "snagyn", "landcover_abbr", "stratum")]

table(pp$stratum)
#There are only 2 points in the tailwater zone, so let's remove these for the purpose of this analysis.
pp <- droplevels(pp[pp$stratum != "TWZ",])
table(pp$stratum)

    #none of these should have negative values
    #change negative values to NA's
    for(i in 1:ncol(pp)){
      pp[,i][pp[,i] < 0] <- NA
    }
    
#example scatterplot matrix
    pairs(pp[,1:7])

#prepare to run univariate logistic regression on a few of these variables
  vars <- colnames(pp)[-c(30:33)]
  names(vars) <- c("Area of Polygon", "Perimeter of Polygon", "Sinuosity of Polygon", "Avg. Fetch of Polygon", "Max. Depth of Polygon", "Avg. Depth of Polygon", "Total Volume of Aquatic Area Polygon", "Percent of Polygon Covered by Bathymetry", "Area with Depth > 1m", "Length of Poly. Perim. adj. to Wet Forest", "% Poly. Perim. adj. to Wet Forest", "% Poly. Terr. Shoreline adj. to Wet Forest", "Length of Poly. Perim. adj. to Lotic Area", "% Poly. Perim. adj. to Lotic Area", "# Lotic Outlets", "Length of Poly. Perim. adj. to Lentic Area", "% Poly. Perim. adj. to Lentic Area", "# Lentic Outlets", "% Aquatic Vegetation", "% Open Water", "Length of Terrestrial Shoreline", "% Poly. Perim. adj. to Terrestrial Area", "% Poly. Perim. adj. to Aquatic Area", "Length of Wing Dams", "Wing Dam Length per m^2 of Area", "# Wing Dams", "Length of Revetted Area", "% Poly. Perim. with Revetment", "% of Terrestrial Shoreline Revetted")

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
  densityplots_stratum <- vector("list", length(vars))

    #fill the lists
    for(i in 1:length(vars)){
      boxplots[[i]] <- outputlist[[i]]$boxplot
      densityplots[[i]] <- outputlist[[i]]$density_plot
      modelsummaries[[i]] <- outputlist[[i]]$model_summary
      predictions[[i]] <- outputlist[[i]]$predictions
      logitplots[[i]] <- outputlist[[i]]$logit_plot
      densityplots_stratum[[i]] <- outputlist[[i]]$density_plot_stratum
    }
  
  # data frame for model coefficients and p values
  coef.pval <- data.frame(
    index = 1:length(vars),
    var = vars,
    coef = rep(NA, length(vars)),
    pval = rep(NA, length(vars))
  )
      #fill data frame with coefficients and p values
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

#Let's look at the logit plots for the variables that were significant at a 0.05 level.
      indices <- coef.pval$index[!is.na(coef.pval$sig05)]
      logitplots[indices]
      densityplots[indices]
      densityplots[-indices]
      
#Visually assess differences by stratum
      densityplots_stratum
    #would be nice to change this so that the densities add up to 1 within a facet. 
