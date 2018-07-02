load("data/new.ef.Rda")
load("data/pool8.barcodes.Rda")
source("libraries.R")

makelogit <- function(df_source, var_of_interest, cwdfactor, cwd, varname){
  box_plot <- df_source %>% ggplot(aes_string(x = cwdfactor, #make boxplot
                          y = var_of_interest,
                          fill = cwdfactor))+ 
    geom_boxplot()+
    xlab("Coarse Woody Debris Presence")+
    coord_flip()+ #make horizontal
    ggtitle(paste("Boxplot of CWD by ", varname))+ #title based on variable
    scale_fill_manual(name = "CWD Presence",
                      values = c("darkgray", "red"))+ #color by CWD presence
    theme_bw()+
    theme(text = element_text(size=18)) #larger text
  
  density_plot <- df_source %>% ggplot(aes_string(x = var_of_interest, #make density plot
                          color = cwdfactor))+
    geom_line(stat = "density", size = 2)+
    scale_color_manual(name = "Coarse Woody Debris",
                       values = c("black", "red"))+ #color by CWD presence
    guides(color = guide_legend(override.aes = list(size = 6)))+
    theme(text = element_text(size = 18))+
    ggtitle(paste("Distribution of", varname))+ #title by variable name
    ylab("Density")
  
  #make formula for model
  formula2 <- paste(cwd, "~", var_of_interest)
  
  #make logistic model with this formula
  model <- glm(formula = formula2, data = df_source, family = "binomial")

  #get model summary
  model_summary <- summary(model)
  
  #extract the p value
  p_value <- paste("p =", coef(summary(model))[,'Pr(>|z|)'][2])
  
  #define column to use
  xb <- df_source[,var_of_interest]
  
  temp <- as.data.frame(seq(from = min(xb), 
                            to = max(xb), 
                            by = (max(xb)-min(xb))/100)
  )
  names(temp) <- var_of_interest
  predictions <- predict.glm(model,
                            newdata = temp,
                            type = 'response',
                            se.fit = T)
  
  generated_data <- cbind(temp, predictions) #generate data to plot curve
  
  fit_var <- "fit" #define fit variable
  logit_plot <- generated_data %>% ggplot(aes_string(x = var_of_interest, y = fit_var))+
    stat_smooth(method = glm,
                formula = y~x,
                method.args = list(family = "binomial"),
                size = 1.5,
                color = "black", 
                se = TRUE,
                level = 0.7)+
    labs(x = varname,
         y = "Probability of CWD presence",
         title = paste("Probability of CWD presence by", varname))+
    scale_y_continuous(limits = c(0,1))+
    theme_bw()+
    theme(text = element_text(size=20))
  
  return(list(boxplot = box_plot,
              density_plot = density_plot, 
              model = model, 
              model_summary = model_summary, 
              p_value = p_value, 
              predictions = predictions, 
              temp = temp, 
              generated_data = generated_data,
              logit_plot = logit_plot))
}

a <- makelogit(df_source = new.ef, var_of_interest = "pct_terr", cwdfactor = "snagyn", cwd = "snag", varname = "Percent Terrestrial Shoreline")
a$logit_plot

