## Modeling

###########
# Some plots of the different barcodes to see relationships between CWD presence and other variables
library(plyr)
library(dplyr)
pool8.barcodes <- read.csv("pool8.barcodes.csv")
#Boxplot of current by CWD
pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=current, fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Current by CWD Presence")+
  xlab("Coarse Woody Debris Presence")+
  ylab("Current")+
  guides(fill = FALSE)

#Boxplot of log(total fish count) by CWD
logfct.cwd <- pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=log(totfishc), fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Log of Fish Count by CWD Presence")+
  xlab("Coarse Woody Debris Presence")+
  ylab("Log of Total Number of Fish Caught")+
  guides(fill = FALSE)
logfct.cwd
#ggsave(filename = "logfct.cwd.png", plot = logfct.cwd, dpi = 500)

# Make a vector of labels for the site types
labels <- c(prim.rand = "Primary Random Site", alt.rand = "Alternate Random Site", subj.perm = "Subjective Permanent Site")
# Boxplot of log(total fish count) by CWD, split by site type
logfct.cwd.facet <- pool8.barcodes %>% filter(is.na(snag) == FALSE) %>% 
  ggplot(aes(x=snag, y=log(totfishc), fill = snag)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#600000", "#ff0000"))+
  ggtitle("Log of Fish Count by CWD Presence")+
  xlab("Coarse woody debris presence, split by site type")+
  ylab("Log of Total Number of Fish Caught")+
  facet_wrap(~sitetype, labeller = labeller(sitetype = labels))+
  guides(fill = FALSE)
logfct.cwd.facet
#ggsave(filename = "logfct.cwd.facet.png", plot = logfct.cwd.facet, dpi = 500)

# have this suggestion from here for a loess plot: https://stats.stackexchange.com/questions/45444/how-do-you-visualize-binary-outcomes-versus-a-continuous-predictor
loess <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = lat, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.2, pch = 20)+
  stat_smooth(method = "loess", color = "darkred", size = 1.5) +
  xlab("N <--    (Latitude)    --> S") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 CWD Presence by Latitude")
#ggsave(filename = "loess.latitude.png", plot = loess, dpi = 500)
# can also use scatter.smooth
# with(pool8.barcodes, scatter.smooth(snag~lat))

# loess plot over time (by date)
ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = sdate, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.3, pch = 20)+
  stat_smooth(method = "loess", color = "blue", size = 1) +
  xlab("Date") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 snag presence by date")+
  theme_bw()

substrt.cwd <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$substrt),], aes(x = substrt))+
  geom_bar(aes(fill = substrt))+
  scale_fill_manual(name = "Substrate", labels = c("Silt", "Silt/Clay/Little Sand", "Sand/Mostly Sand", "Gravel/Rock/Hard Clay"), values=wes_palette("Darjeeling1"))+
  facet_wrap(~snag)+
  ylab("Number of Sampling Events")+
  xlab("Substrate Type")+
  ggtitle("Substrate Distributions by CWD Presence")+
  theme(axis.text.x = element_blank())
substrt.cwd
#ggsave(filename = "substrt.cwd.png", plot = substrt.cwd, dpi = 500)

## Logistic regression for predicting CWD
library(car)

#Turbidity
# convert `snag` to numeric for the purposes of logistic regression
pool8.barcodes$snag <- as.numeric(as.character(pool8.barcodes$snag))

# logistic model with turbidity
msecchi <- glm(snag~secchi, 
               data = pool8.barcodes[!is.na(pool8.barcodes$snag) 
                                     & is.na(pool8.barcodes$s_qf),], 
               family = binomial)
summary(msecchi)
# marginal model plots
mmps(msecchi, main = "Marginal Model Plots for Turbidity Measurements")
# note that we should remove the "Linear Predictor" text from this plot

# plot results of Secchi/Turbidity model
secchi <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & is.na(pool8.barcodes$s_qf),], 
                 aes(x = secchi, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "dodgerblue2")+
  ggtitle("CWD Presence vs. Water Clarity")+
  xlab("more turbidity <--     (Secchi disk measurement)     --> less turbidity")+
  ylab("CWD Presence/Absence")+
  theme(plot.title = element_text(size = 18), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
secchi
#ggsave(filename = "secchi.png", plot = secchi, dpi = 500)


#Depth
mdepth <- glm(snag~depth, data = pool8.barcodes, family = binomial)
summary(mdepth)
mmps(mdepth)

ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$depth),], 
       aes(x = depth, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Depth")

#Temperature
mtemp <- glm(snag~temp, data = pool8.barcodes, family = binomial)
summary(mtemp)
mmps(mtemp)

ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$temp),], 
       aes(x = temp, y = (snag)))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Temperature")
