## Modeling

source("libraries.R")
###########
# Some plots of the different barcodes to see relationships between CWD presence and other variables

load("pool8.barcodes.Rda")
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
Loess <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = lat, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.2, pch = 20)+
  stat_smooth(method = "loess", color = "darkred", size = 1.5) +
  xlab("N <--    (Latitude)    --> S") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 CWD Presence by Latitude")
#ggsave(filename = "loess.latitude.png", plot = Loess, dpi = 500)
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
# trying both linear and quadratic terms
mdepth <- glm(snag~depth, data = pool8.barcodes, family = binomial)
qdepth <- glm(snag~I(depth^2), data = pool8.barcodes, family = binomial)
summary(qdepth)
summary(mdepth)
mmps(mdepth)
mmps(qdepth)


depthvals <- as.data.frame(seq(from = 0, to = 5, by = 0.1))
names(depthvals) <- "depth"
depthvals$preds <- predict(qdepth, depthvals, type = "response") #have to include "response" to make it give you probabilities, not log odds. 

# plot the quadratic logistic regression: don't know how to make this work
ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$depth),], 
       aes(x = depth, y = as.numeric(as.character(snag))))+
  geom_point(alpha = 0.5)+
  geom_line(data = depthvals, aes(x = depth, y = preds), col = "blue", size = 1.5)+
  ggtitle("CWD Presence vs. Depth (Quadratic)")

# plot the regular logistic regression
ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag)& !is.na(pool8.barcodes$depth),], 
       aes(x = depth, y = snag))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Depth")

#Temperature
mtemp <- glm(snag~temp, data = pool8.barcodes, family = binomial)
summary(mtemp)
mmps(mtemp)

ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$temp),], 
       aes(x = temp, y = snag))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  ggtitle("CWD Presence vs. Temperature")

# Make logistic model with all predictors
fullmod <- glm(snag~depth + I(depth^2) + current + stageht + wingdyke + substrt, data = pool8.barcodes, family = binomial)
summary(fullmod)

# Make new model with only the ones that were significant in fullmod
partialmod <- glm(snag~ wingdyke + substrt, data = pool8.barcodes, family = binomial)
summary(partialmod)
# notice that none of the levels of substrt are significant anymore. 

plot(pool8.barcodes$sdat[years(pool8.barcodes$sdat) == 1995], pool8.barcodes$temp[years(pool8.barcodes$sdat) == 1995], xlab = "Month/Day/1995", ylab = expression("Temperature in "*degree*"C"))

ggplot(data = pool8.barcodes %>% filter(period %in% c(1,2,3), !is.na(temp)), 
       aes(x = factor(period), y = temp)) +
  geom_point(alpha = .1) +
  xlab("Period") +
  ylab("Temperature") +
  scale_x_discrete(labels=c("1" = "Jun 15 - Jul 31", "2" = "Aug 1 - Sep 14", "3" = "Sep 15 - Oct 31"))

### Investigating aquatic habitats

#someone on stackoverflow made a palette with 25 distinct colors:
c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")

pool8.barcodes %>% filter(!is.na(snag)) %>% filter(!is.na(aqua_shortname)) %>% ggplot(aes(x = aqua_shortname))+
  geom_bar(aes(fill = aqua_shortname))+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    labels = levels(pool8.barcodes$aqua_shortname),
                    values = c25)+
  facet_wrap(~snag)+
  ylab("Number of Sampling Events")+
  xlab("Aquatic Habitat Type")+
  ggtitle("Aquatic Habitat Types by CWD Presence")+
  theme(axis.text.x = element_blank())
#not sure why I can't seem to exclude the NOPH rows with filter(aqua_shortname != "NOPH"). Especially weird because the same filtering syntax works fine in mod1 below. Help?

# Modeling based on aquatic habitat type and wingdyke
mod1 <- glm(snag~ wingdyke + aqua_shortname + depth, data = pool8.barcodes %>% filter(aqua_shortname != "NOPH"), family = binomial)
summary(mod1)

# Making a plot of this model, following this tutorial: https://blogs.uoregon.edu/rclub/2016/04/14/plotting-logistic-regressions-part-3/
depth_range <- seq(from = 0, to = 5, by=.1)
generated_data <- as.data.frame(expand.grid(depth = depth_range, wingdyke = c(0, 1), aqua_shortname = levels(pool8.barcodes$aqua_shortname)[-10] )) #shouldn't exclude this by index number but I can't figure out how to exclude it by name!!
head(generated_data)
generated_data$prob <- predict(mod1, newdata = generated_data, type = 'response')
head(generated_data) 

# make `wingdyke` into a factor
generated_data$wingdyke_level <- factor(generated_data$wingdyke, labels = c("no wingdam/dyke", "wingdam/dyke present"), ordered=T)
summary(generated_data)

# actually plot the model: this is where ggplot2 comes in handy.
plot.data <- generated_data
# check out the plotting data
head(plot.data)

#facet by aquatic habitat type, color by wingdyke presence
ggplot(plot.data, aes(x=depth, y=prob, color=wingdyke_level)) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(values = c("dodgerblue2", "red"))+
  labs(x="Water depth (meters)", y="P(CWD)", title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~aqua_shortname)+
  theme_bw()

# let's try flipping it, so the facets are by wingdyke presence level and the lines are color coded by aquatic habitat type.
ggplot(plot.data, aes(x=depth, y=prob, color=aqua_shortname)) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(values = brewer.pal(12,"Paired"))+
  labs(x="Water depth (meters)", y="P(CWD)", title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~wingdyke_level)+
  theme_bw()

#Interaction term between wingdyke and aqua_shortname is not helpful
mod2 <- glm(snag~ wingdyke + aqua_shortname + depth + wingdyke*aqua_shortname, data = pool8.barcodes %>% filter(aqua_shortname != "NOPH"), family = binomial)
summary(mod2)

#Interaction term between depth and wingdyke looks a lot better.
mod3 <- glm(snag~ wingdyke + aqua_shortname + depth + wingdyke*depth, data = pool8.barcodes %>% filter(aqua_shortname != "NOPH"), family = binomial)
summary(mod3)

plot.data3 <- generated_data
plot.data3$prob <- predict(mod3, newdata = generated_data, type = 'response')

ggplot(plot.data3, aes(x=depth, y=prob, color=aqua_shortname)) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(values = brewer.pal(12,"Paired"))+
  labs(x="Water depth (meters)", y="P(CWD)", title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~wingdyke_level)+
  theme_bw()
# this looks really different, and we should think about how to interpret it. But I'm also not sure this makes biological sense, since maybe wingdams/dykes are only in shallow areas?
ggplot(data = pool8.barcodes, aes(x = depth, y = wingdyke))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
