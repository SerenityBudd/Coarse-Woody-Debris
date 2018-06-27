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
  xlab("S <--    (Latitude)    --> N") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 CWD Presence by Latitude")
Loess
#ggsave(filename = "loess.latitude.png", plot = Loess, dpi = 500)
# can also use scatter.smooth
# with(pool8.barcodes, scatter.smooth(snag~lat))

# loess plot over time (by date)
timeplot <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], aes(x = sdate, y = as.numeric(as.character(snag)))) +
  geom_point(size = 2, alpha = 0.3, pch = 20)+
  stat_smooth(method = "loess", color = "blue", size = 1) +
  xlab("Date") +
  ylab("Snag Presence") +
  ggtitle("Pool 8 snag presence by date")+
  theme_bw()
timeplot 

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
data2 <- pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$depth),]
data2$snag <- as.numeric(as.character(data2$snag))

mdepth <- glm(snag~depth, data = data2, family = binomial)
qdepth <- glm(snag~I(depth^2), data = data2, family = binomial)
summary(qdepth)
summary(mdepth)
mmps(mdepth)
mmps(qdepth)

depthvals <- as.data.frame(seq(from = 0, to = 5, by = 0.1))
names(depthvals) <- "depth"
depthvals$preds <- predict(qdepth, depthvals, type = "response", se.fit = T)$fit
depthvals$se <- predict(qdepth, depthvals, type = "response", se.fit = T)$se.fit
#have to include "response" to make it give you probabilities, not log odds. 

# plot the quadratic logistic regression: don't know how to make this work
ggplot(data = data2, 
       aes(x = depth, y = snag))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = glm, 
              formula = y ~ I(x^2), 
              method.args = list(family = "binomial"),
              size = 1.5)+
  ggtitle("CWD Presence vs. Depth (Quadratic)")+
  xlab("Depth (meters)")+
  ylab("Coarse Woody Debris Presence")+
  theme(text = element_text(size=20))

# plot the regular logistic regression
ggplot(data = data2, 
       aes(x = depth, y = snag))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = glm, 
              formula = y ~ x, 
              method.args = list(family = "binomial"),
              size = 1.5)+
  ggtitle("CWD Presence vs. Depth")+
  xlab("Depth (meters)")+
  ylab("Coarse Woody Debris Presence")+
  theme(text = element_text(size=20))

#Temperature
data3 <- pool8.barcodes[!is.na(pool8.barcodes$snag) & !is.na(pool8.barcodes$temp),]
data3$snag <- as.numeric(as.character(data3$snag))

mtemp <- glm(snag~temp, data = data3, family = binomial)
summary(mtemp)
mmps(mtemp)

ggplot(data = data3, 
       aes(x = temp, y = snag))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = glm, 
              formula = y ~ x, 
              method.args = list(family = "binomial"),
              size = 1.5)+
  ggtitle("CWD Presence vs. Water Temperature")+
  xlab("Water Temperature (ºC)")+
  ylab("Coarse Woody Debris Presence")+
  theme(text = element_text(size=20))

# Make logistic model with all predictors
fullmod <- glm(snag~depth + I(depth^2) + current + stageht + wingdyke + substrt, data = pool8.barcodes, family = binomial)
summary(fullmod)

# Make new model with only the ones that were significant in fullmod
partialmod <- glm(snag~ wingdyke + substrt, data = pool8.barcodes, family = binomial)
summary(partialmod)
# notice that none of the levels of substrt are significant anymore. 

#Make plot of water temperature over one year
data1995 <- 
plot(pool8.barcodes$sdat[years(pool8.barcodes$sdat) == 1995], pool8.barcodes$temp[years(pool8.barcodes$sdat) == 1995], xlab = "Month/Day/1995", ylab = expression("Temperature in ºC"), main = "Water temperature over time, 1995")

#ggplot temperature graph showing sampling periods
ggplot(data = pool8.barcodes %>% filter(period %in% c(1,2,3), !is.na(temp)), 
       aes(x = factor(period), y = temp)) +
  geom_point(alpha = .1) +
  xlab("Period") +
  ylab("Temperature") +
  scale_x_discrete(labels=c("1" = "Jun 15 - Jul 31", "2" = "Aug 1 - Sep 14", "3" = "Sep 15 - Oct 31"))+
  theme(text = element_text(size=20))

### Investigating aquatic habitats

# establish color scale
source("color_schemes.R")

#the colors are still messed up on this graph!!
pool8.barcodes %>% filter(!is.na(snag)) %>% filter(!is.na(aqua_desc)) %>% ggplot(aes(x = aqua_desc))+
  geom_bar(aes(fill = aqua_desc))+
  scale_fill_manual(name = "Aquatic Habitat Type", 
                    labels = levels(pool8.barcodes$aqua_desc),
                    values = myColors)+
  facet_wrap(~snag)+
  ylab("Number of Sampling Events")+
  xlab("Aquatic Habitat Type")+
  ggtitle("Aquatic Habitat Types by CWD Presence")+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_blank())
#This color-coding is still off and I don't know why.

# Modeling based on aquatic habitat type and wingdyke and depth
mod1 <- glm(snag ~ wingdyke + aqua_desc + depth, data = pool8.barcodes, family = binomial)
summary(mod1)

# Making a plot of the habitat/wingdyke/depth model, following this tutorial: https://blogs.uoregon.edu/rclub/2016/04/14/plotting-logistic-regressions-part-3/
depth_range <- seq(from = 0, to = 5, by=.1)
generated_data <- as.data.frame(expand.grid(depth = depth_range, wingdyke = c(0, 1), aqua_desc = levels(droplevels(pool8.barcodes)$aqua_desc)))
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
  labs(x="Water depth (meters)", y="Probability of CWD presence", title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~aqua_desc)+
  theme_bw()
#overlay data on this to show where the points fall with respect to depth (rug?)

# let's try flipping it, so the facets are by wingdyke presence level and the lines are color coded by aquatic habitat type.
bigplot <- ggplot(plot.data, aes(x=depth, y=prob, color=aqua_desc)) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(name = "Aquatic Habitat Type", values = myColors)+
  labs(x="Water depth (meters)", y="P(CWD)", 
       title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~wingdyke_level)+
  theme_bw()+
  theme(axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20),
        title = element_text(size = 20))
# ggsave(file = "bigplot.png", plot = bigplot, dpi = 500)
######################

#Interaction term between wingdyke and aqua_desc is not helpful
mod2 <- glm(snag~ wingdyke + aqua_desc + depth + wingdyke*aqua_desc, data = pool8.barcodes, family = binomial)
summary(mod2)

#Interaction term between depth and wingdyke looks a lot better.
mod3 <- glm(snag~ wingdyke + aqua_desc + depth + wingdyke*depth, data = pool8.barcodes, family = binomial)
summary(mod3)

plot.data3 <- generated_data
plot.data3$prob <- predict(mod3, newdata = generated_data, type = 'response')

#Plot of P(CWD) vs depth and aquahab type, accounting for wingdyke/depth interaction
ggplot(plot.data3, aes(x=depth, y=prob, color=aqua_desc)) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(values = myColors)+
  labs(x="Water depth (meters)", y="Probability of CWD presence", title="Probability of Coarse Woody Debris Presence") +
  facet_wrap(~wingdyke_level)+
  theme_bw()
# this looks really different, and we should think about how to interpret it. But I'm also not sure this makes biological sense, since maybe wingdams/dykes are only in shallow areas?
#where are wingdams located?
ggplot(data = pool8.barcodes, aes(x = depth, y = wingdyke))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# Model with just depth and aquatic habitat type
# Exclude non-aquatic areas, and habitat types where only one barcode occurred.
# Not sure why they sampled in non-aquatic areas. 

mod4 <- glm(snag ~ depth + aqua_desc, 
            data = pool8.barcodes, 
              family = binomial)
summary(mod4)
#depth is not significant in this model.

#Investigate aquatic habitat strata
load("newdat.Rda")
source("libraries.R")
source("color_schemes.R")

# Bar Chart of Number of Sampling Points by Stratum
newdat %>% filter(!is.na(snag)) %>% filter(!is.na(stratum_name)) %>% ggplot(aes(x = stratum_name))+
  geom_bar(aes(fill = stratum_name))+
  scale_fill_manual(name = "Aquatic Habitat Stratum", 
                    labels = names(strataColors_distinct),
                    values = strataColors_distinct)+
  facet_wrap(~snag)+
  ylab("Number of Sampling Events")+
  xlab("Aquatic Habitat Stratum")+
  ggtitle("Aquatic Habitat Strata by CWD Presence")+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_blank())

# Points by year
by_year <- data.frame(year = sort(unique(newdat$year)),
                      CWDpoints = NA,
                      totpoints = NA,
                      propCWD = NA)

for(i in 1:nrow(by_year)){
  by_year$CWDpoints[i] <- sum(newdat$snag[newdat$year == by_year$year[i]])
}

for(i in 1:nrow(by_year)){
  by_year$totpoints[i] <- nrow(newdat[newdat$year == by_year$year[i],])
}

by_year$propCWD <- round(by_year$CWDpoints / by_year$totpoints, 2)
by_year

#Proportion of points with CWD by year
ggplot(data = by_year, aes(x = year, y = propCWD)) +
  geom_line(size = 1.5)+
  geom_point(size = 3, col = "red")+
  ggtitle("Proportion of Sites with CWD by Sampling Year")+
  xlab("Year of Sampling") +
  ylab("Proportion of Sites with CWD")+
  theme(text = element_text(size=20))

#CWD points and total points by year
ggplot(data = by_year, aes(x = year))+
  geom_line(aes(y = totpoints), size = 1.5)+
  geom_line(aes(y = CWDpoints), size = 1.5, col = "red")+
  ggtitle("# CWD Points and # Points Sampled by Year")
#not sure how to make a legend for this one

#CWD points by total points sampled
with(by_year, plot(CWDpoints~totpoints, pch = 1, col = "darkred", main = "CWD Points vs. Total Points Sampled"))

#Table by stratum and year
stratum.year <- as.data.frame(newdat %>% 
                                group_by(year, stratum_name) %>% 
                                summarize(totpoints = n(), 
                                          CWDpoints = sum(snag), 
                                          propCWD = round(sum(snag/n()), 4))
                              )

#Number of points by stratum and year
ggplot(data = stratum.year, aes(x = year, y = totpoints, color = stratum_name))+
  geom_line(size = 1.5)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Number of Points Sampled by Stratum Over Time")+
  ylab("Number of Sampling Points")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))

#Proportion of CWD by Stratum and Year
ggplot(data = stratum.year, aes(x = year, y = propCWD, color = stratum_name))+
  geom_smooth(size = 1.5, se = F)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Proportion of Points with CWD by Year")+
  ylab("Proportion of Points with CWD")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))
  
  
  