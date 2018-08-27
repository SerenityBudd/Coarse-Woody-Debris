source("CWD_and_Fish/sunfish-preferences.R")

#############################################
## tables for the multiplicative impact of wood on abundance
multmodcoeff <- rbind(t(BLGL.m1coeff * 1.1397204),
                      t(GNSF.m1coeff * 3.0705374),
                      t(SMBS.m1coeff * 1.3609800))[,7:10]
#write.csv(multmodcoeff, "data/multmodcoeff.csv")

#############################################
## tables for the additive impact of wood on abundance
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
    ggtitle("Plot of the interaction effects of wood by aquatic habitat stratum on Abundance")
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
    theme(text = element_text(size = 20)) + 
    ggtitle("Plot of the effects of wood on abundance")
}


catty_plot2(BKCP.m2)
catty_plot2(LMBS.m2)
catty_plot2(OSSF.m2)
catty_plot2(RKBS.m2)
catty_plot2(WTCP.m2)

