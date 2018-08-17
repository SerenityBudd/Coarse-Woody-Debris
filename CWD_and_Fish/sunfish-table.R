source("CWD_and_Fish/newthings.R")


#############################################
## plot effects of wood on abundance

catty_plot1 <- function(MODEL) {
  cat_plot(model = MODEL, pred = snag, errorbar.width = 0, interval = T, int.type = "confidence", y.label = "Abundance") +
    theme_bw() +
    scale_x_discrete("Wood", labels = c("Absent", "Present")) +
    theme(text = element_text(size = 20)) #+
 # ggtitle("Plot of the effects of wood on abundance")
}


catty_plot1(BKCP.m1)
catty_plot1(BLGL.m1)
catty_plot1(GNSF.m1)
catty_plot1(LMBS.m1)
catty_plot1(OSSF.m1)
catty_plot1(RKBS.m1)
catty_plot1(SMBS.m1)
catty_plot1(WTCP.m1)


#############################################
## plot interaction effects of wood by stratum on abundance
catty_plot2 <- function(model) {
  cat_plot(model, pred = stratum_name, modx = snag, errorbar.width = 0,
           y.label = "Abundance", modx.labels = c("Absent", "Present"),
           legend.main = "Wood") +
    theme_bw() +
    scale_x_discrete("Aquatic Habitat", labels = c("BWC-S", "IMP-S", "MCB-U", "MCB-W", "SCB")) +
    scale_color_manual(values = c("red", "blue"))+
    theme(text = element_text(size = 20)) #+
  #ggtitle("Plot of the Effects of Aquatic Habitat Stratum and Wood on Abundance")
}


catty_plot2(BKCP.m2)
catty_plot2(BLGL.m2)
catty_plot2(GNSF.m2)
catty_plot2(LMBS.m2)
catty_plot2(OSSF.m2)
catty_plot2(RKBS.m2)
catty_plot2(SMBS.m2)
catty_plot2(WTCP.m2)


#############################################
## my function to plot wood effects on abundance
catplot <- function(MODEL) {
  ggplot(data = MODEL, aes(y = N, x = snag)) + 
    geom_point(aes(x = "No", y = exp(MODEL$coefficients)[1]), col = "red") + 
    geom_point(aes(x = "Yes", y=exp(BKCP.m2$coefficients)[1] * exp(BKCP.m2$coefficients)[6]), col = "red")# +
  #  geom_errorbar( width = 0)
}
  

catplot(BKCP.m2)
catplot(BLGL.m2)
catplot(GNSF.m2)
catplot(LMBS.m2)
catplot(OSSF.m2)
catplot(RKBS.m2)
catplot(SMBS.m2)
catplot(WTCP.m2)

#############################################
## other attempts of ggploting


geom_point(aes(x = "No", y = exp(BKCP.m2$coefficients)[1]), col = "red") + 
  geom_point(aes(x = "Yes", y= exp(BKCP.m2$coefficients[1] + BKCP.m2$coefficients[6])), col = "red") +
  geom_point(aes(x = "No", y = 1.45151179), col = "red") + 
  geom_point(aes(x = "Yes", y= 1.09600396 * 1.713392), col = "red") +
  
  geom_point(aes(x = "No", y = 2.0070532), col = "red")+
  geom_point(aes(x = "Yes", y= 1.5687923 * 1.713392), col = "red")  
