#Stratum and one other variable: logistic regression to predict CWD
library(dplyr)
library(ggplot2)
source("color_schemes.R")

load("data/new.ef.Rda")

makelogit.interact <- function(df_source, var_of_interest, cwdfactor, cwd, varname, stratum){
  #make density plot
  density_plot_stratum <- df_source %>% ggplot(aes_string(x = var_of_interest,
                                                  color = cwdfactor))+
    geom_line(stat = "density", size = 2)+
    scale_color_manual(name = "Coarse Woody Debris",
                       values = c("black", "red"))+ #color by CWD presence
    guides(color = guide_legend(override.aes = list(size = 6)))+
    theme(text = element_text(size = 18))+
    ggtitle(paste("Distribution of", varname))+ #title by variable name
    ylab("Density")+
    xlab(varname)+
    facet_wrap(~stratum)
  
  #make formula for model
  formula3 <- paste0(cwd, "~", var_of_interest, "+", var_of_interest, "*", stratum)
  
  #make logistic model with this formula
  model <- glm(formula = formula3, data = df_source, family = "binomial")
  
  #get model summary
  model_summary <- summary(model)
  
  #define column to use
  xb <- df_source[,var_of_interest]
  xs <- df_source[,stratum]
  
  temp <- seq(from = min(xb, na.rm = T), 
              to = max(xb, na.rm = T), 
              by = (max(xb, na.rm = T) - min(xb, na.rm = T))/100
  )
  
  generated_data <- as.data.frame(expand.grid(var = temp, stratum = levels(xs)))
  names(generated_data) <- c(var_of_interest, stratum)
  
  fit <- predict.glm(model,
                     newdata = generated_data,
                     type = 'response',
                     se.fit = TRUE)
  
  results <- cbind(generated_data, fit)
  
  fit_var <- "fit" #define fit variable
  #make logit plot
  logit_plot <- results %>% 
    ggplot(aes_string(x = var_of_interest, 
                      y = fit_var,
                      color = stratum))+
    #geom_ribbon(aes(ymin = fit - se.fit, 
                    #ymax = fit + se.fit), 
                #fill = rgb(0, 0, 0, 0.2))+
    geom_line(size = 1)+
    labs(x = varname,
         y = "Probability of CWD presence",
         title = paste("Prob. of CWD by", varname))+
    scale_y_continuous(limits = c(0,1))+
    theme_bw()+
    theme(text = element_text(size=20))
  
  return(list(densityplotstratum = density_plot_stratum,
              model = model, 
              model_summary = model_summary,
              generated_data = generated_data,
              logit_plot = logit_plot))
}

a <- makelogit.interact(df_source = new.ef, var_of_interest = "pct_terr", cwdfactor = "snagyn", cwd = "snag", varname = "Percent Terrestrial Shoreline", stratum = "stratum")
a$logit_plot
