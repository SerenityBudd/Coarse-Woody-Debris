source("CWD_and_Fish/sunfish-preferences.R")

#############################################
## tables for the multiplicative impact of wood on abundance
multmodcoeff <- rbind(t(BLGL.m1coeff * 1.1397204),
                      t(GNSF.m1coeff * 3.0705374),
                      t(SMBS.m1coeff * 1.3609800))[,7:10]
#write.csv(multmodcoeff, "data/multmodcoeff.csv")


addmodcoeff <- rbind(t(BKCP.m2coeff),
                     t(LMBS.m2coeff),
                     t(OSSF.m2coeff),
                     t(RKBS.m2coeff),
                     t(WTCP.m2coeff))[,6]

#write.csv(addmodcoeff, "data/addmodcoeff.csv")


#############################################
## plot interaction effects of wood by stratum on abundance
catty_plot1 <- function(model) {
  cat_plot(model, pred = stratum_name, modx = snag, errorbar.width = 0,
           y.label = "Abundance", modx.labels = c("Absent", "Present"),
           legend.main = "Wood", color.class = c("red", "blue")) +
    theme_bw() +
    scale_x_discrete("Aquatic Habitat", labels = c("BWC-S", "IMP-S", "MCB-U", "MCB-W", "SCB")) +
    theme(text = element_text(size = 20)) +
    coord_flip()
  #ggtitle("Plot of the Effects of Aquatic Habitat Stratum and Wood on Abundance")
}


catty_plot1(BLGL.m1)
catty_plot1(GNSF.m1)
catty_plot1(SMBS.m1)


#############################################
## plot effects of wood on abundance

catty_plot2 <- function(MODEL) {
  cat_plot(model = MODEL, pred = snag, errorbar.width = 0, interval = T, int.type = "confidence", y.label = "Abundance") +
    theme_bw() +
    scale_x_discrete("Wood", labels = c("Absent", "Present")) +
    theme(text = element_text(size = 20)) #+
  # ggtitle("Plot of the effects of wood on abundance")
}


catty_plot2(BKCP.m2)
catty_plot2(LMBS.m2)
catty_plot2(OSSF.m2)
catty_plot2(RKBS.m2)
catty_plot2(WTCP.m2)


#############################################
## my function to plot wood effects on abundance -- Sanity Check

cat_plot(model = BKCP.m2, pred = snag, errorbar.width = 0, interval = T, int.type = "confidence", y.label = "Abundance") + 
    geom_point(aes(x = "No", y = exp(BKCP.m2$coefficients)[1]), col = "red") + 
    geom_point(aes(x = "Yes", y=exp(BKCP.m2$coefficients)[1] * exp(BKCP.m2$coefficients)[6]), col = "red") +
    geom_point(aes(x = "No", exp(confint(BKCP.m2)[1,1])), col = "red") + 
    geom_point(aes(x = "No", exp(confint(BKCP.m2)[1,2])), col = "red") + 
    geom_point(aes(x = "Yes", 2.15175 * 1.119548), col = "red") +
    geom_point(aes(x = "Yes", 2.15175 / 1.119548), col = "red") 

exp(BKCP.m2$coefficients)[1] * exp(BKCP.m2$coefficients)[6]
# 2.15175
exp(sqrt((summary(BKCP.m2)$coefficients[1, 2])^2 + (summary(BKCP.m2)$coefficients[6, 2])^2 + 2*vcov(BKCP.m2)[6,1]) * 1.96)
# 1.119548
