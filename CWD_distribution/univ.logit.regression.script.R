makelogit <- function(df, var, cwdfactor, cwd, varname){
  bp <- df %>% ggplot(aes_string(x = cwdfactor,
                          y = var,
                          fill = cwdfactor))+
    geom_boxplot()+
    xlab("Coarse Woody Debris Presence")+
    coord_flip()+
    ggtitle(paste("Boxplot of CWD by ", varname))+
    scale_fill_manual(name = "CWD Presence",
                      values = c("darkgray", "red"))+
    theme_bw()+
    theme(text = element_text(size=18))
  
  dp <- df %>% ggplot(aes_string(x = var,
                          color = cwdfactor))+
    geom_line(stat = "density", size = 2)+
    scale_color_manual(name = "Coarse Woody Debris",
                       values = c("black", "red"))+
    guides(color = guide_legend(override.aes = list(size = 6)))+
    theme(text = element_text(size = 18))+
    ggtitle(paste("Distribution of", varname))+
    ylab("Density")
  
  return(list(bp, dp))
}

mod_pct_terr <- glm(snag~pct_terr, data = new.ef, family = "binomial")
summary(mod_pct_terr)
#ooh wow look at that coefficient and that p-value!!

#generate data to plot model with line
pct_terr <- data.frame(pct_terr = seq(from = 0, to = 100, by = 1))
predictions <- as.data.frame(predict(mod_pct_terr, 
                                     newdata = pct_terr, 
                                     type = 'response', 
                                     se.fit = T))
generated_data <- cbind(pct_terr, predictions)
#plot model
generated_data %>% ggplot(aes(x = pct_terr, 
                              y = fit)) + 
  stat_smooth(method = glm, 
              formula = y ~ x, 
              method.args = list(family = "binomial"),
              size = 1.5,
              color = "black")+
  labs(x="% Terrestrial shoreline perimeter", 
       y="Probability of CWD presence", 
       title="Probability of Coarse Woody Debris Presence")+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  theme(text = element_text(size=20))
