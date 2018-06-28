# Week 4 sandbox, 6/25/18 - 6/29/18
source("libraries.R")
source("color_schemes.R")
load("data/new.ef.Rda")
load("data/pool8.barcodes.Rda")

#Map electrofishing points by stratum type
(mbs <- ggplot(data = new.ef, aes(x = lon, y = lat, color = stratum_name))+
  geom_point(size = 0.7)+
  coord_fixed(1.2)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Sampling Points by Aquatic Habitat Stratum")+
  scale_x_continuous(limits = c(-91.35, -91.15))
)

  #By year
  (mbs_y <- mbs + 
      facet_wrap(~year)
  )

# Bar Chart of Number of Sampling Points by Stratum
nss <- new.ef %>% filter(!is.na(snag)) %>% filter(!is.na(stratum_name)) %>% ggplot(aes(x = stratum_name))+
  geom_bar(aes(fill = stratum_name))+
  scale_fill_manual(name = "Aquatic Habitat Stratum", 
                    labels = names(strataColors_distinct),
                    values = strataColors_distinct)+
  ylab("Number of Sampling Events")+
  xlab("Aquatic Habitat Stratum")+
  ggtitle("Aquatic Habitat Strata by CWD Presence")+
  theme(axis.text.x = element_blank())
nss + theme(text = element_text(size=20))

  #by snag
  (nss_s <- nss+
    facet_wrap(~snag)+
    theme(text = element_text(size=20))
  )

  #by year
  (nss_y <- nss+
    facet_wrap(~year)
  )

# Points by year
by_year <- data.frame(year = sort(unique(new.ef$year)),
                      CWDpoints = NA,
                      totpoints = NA,
                      propCWD = NA)
  # CWDpoints
  for(i in 1:nrow(by_year)){
    by_year$CWDpoints[i] <- sum(new.ef$snag[new.ef$year == by_year$year[i]])
  }
  # total points
  for(i in 1:nrow(by_year)){
    by_year$totpoints[i] <- nrow(new.ef[new.ef$year == by_year$year[i],])
  }
  # CWD proportion
  by_year$propCWD <- round(by_year$CWDpoints / by_year$totpoints, 2)
  # view table
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
(stratum.year <- as.data.frame(new.ef %>% 
                                group_by(year, stratum_name) %>% 
                                summarize(totpoints = n(), 
                                          CWDpoints = sum(snag), 
                                          propCWD = round(sum(snag/n()), 4))
  )
)

#Number of points by stratum and year
(npsy <- ggplot(data = stratum.year, aes(x = year, y = totpoints, color = stratum_name))+
  geom_line(size = 1.5)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Number of Points Sampled by Stratum Over Time")+
  ylab("Number of Sampling Points")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))
)

#Proportion of CWD by Stratum and Year
(prop.cwd_sy <- ggplot(data = stratum.year[stratum.year$stratum_name != "Impounded--Offshore",], 
       aes(x = year, 
           y = propCWD, 
           color = stratum_name))+
  geom_line(size = 1.5)+
  scale_color_manual(name = "Aquatic Habitat Stratum", 
                     values = strataColors_distinct)+
  ggtitle("Proportion of Points with CWD by Year")+
  ylab("Proportion of Points with CWD")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(size=20))
)
      
      #same thing but smoothed
      (prop.cwd_sy_s <- ggplot(data = stratum.year[stratum.year$stratum_name != "Impounded--Offshore",], 
             aes(x = year, 
                 y = propCWD, 
                 color = stratum_name))+
        geom_smooth(size = 1.5, se = FALSE)+
        scale_color_manual(name = "Aquatic Habitat Stratum", 
                           values = strataColors_distinct)+
        ggtitle("Proportion of Points with CWD by Year")+
        ylab("Proportion of Points with CWD")+
        xlab("Year")+
        theme_bw()+
        theme(text = element_text(size=20))
      )

# How does proportion of CWD change over time at the fixed sites?
  # Just the fixed sites:
  ggplot(data = new.ef %>% filter(sitetype == 2), aes(x = lon, y = lat))+
    geom_point()
  # There seem to be only two fixed sites. 
  # So why are there 80 rows marked as being at a fixed site?
  table(new.ef$sitetype)
  table(new.ef[new.ef$sitetype == 2,]$barcode)
  table(new.ef[new.ef$sitetype == 2,]$year)
  table(droplevels(new.ef[new.ef$sitetype == 2,]$lcode))
  # only two location codes
  length(unique(new.ef[new.ef$sitetype == 2,]$barcode))
  # it appears that each location was sampled 40 times.
  length(unique(new.ef[new.ef$lcode == "M692.7R" & 
                         new.ef$sitetype == 2,]$barcode))
  length(unique(new.ef[new.ef$lcode == "M701.8C" & 
                         new.ef$sitetype == 2,]$barcode))
  
  #plot CWD over time for each fixed site
  s1 <- new.ef[new.ef$lcode == "M692.7R" & new.ef$sitetype == 2,]
  s1_s <- s1 %>% 
    group_by(year) %>% 
    summarize(prop.cwd = sum(snag)/n(),
              site = factor("1"))
  
  s2 <- new.ef[new.ef$lcode == "M701.8C" & new.ef$sitetype == 2,]
  s2_s <- s2 %>%
    group_by(year) %>%
    summarize(prop.cwd = sum(snag)/n(),
              site = factor("2"))

  fixed <- rbind(s1_s, s2_s)
  
# CWD presence at fixed sites over time
  ggplot(data = fixed, aes(x = year, y = prop.cwd, col = site, linetype = site))+
    geom_line(size = 1)+
    geom_point(size = 2)+
    scale_y_continuous(limits = c(0, 1))+
    ggtitle("CWD Presence at Fixed Sites Over Time")
    
  
# Stratum and Landcover Type
table_all <- as.data.frame.matrix(with(new.ef, 
                                       table(stratum_name, landcover_abbr)))
table_prop_all <- table_all/nrow(new.ef)
table_prop_all$stratum <- row.names(table_prop_all)
row.names(table_prop_all) <- NULL
ma <- melt(table_prop_all, id.vars = "stratum")

table_cwd <- as.data.frame.matrix(with(new.ef %>% filter(snag == 1), 
                                       table(stratum_name, landcover_abbr)))
table_prop_cwd <- table_cwd/nrow(new.ef %>% filter(snag == 1))
table_prop_cwd$stratum <- row.names(table_prop_cwd)
row.names(table_prop_cwd) <- NULL
mc <- melt(table_prop_cwd, id.vars = "stratum")

ggplot(data = ma, aes(variable, stratum))+
  geom_tile(aes(fill = value))+
  scale_fill_gradient("Legend label", low = "lightblue", high = "black") +
  theme_bw()+
  ggtitle("Proportion plot for all points")+
  xlab("Nearest Land Cover Type")+
  ylab("Aquatic Stratum")

ggplot(data = mc, aes(variable, stratum))+
  geom_tile(aes(fill = value))+
  scale_fill_gradient("Legend label", low = "lightblue", high = "black") +
  theme_bw()+
  ggtitle("Proportion plot for CWD points")+
  xlab("Nearest Land Cover Type")+
  ylab("Aquatic Stratum")

# Distance to nearest landcover, by snag presence
ggplot(data = new.ef %>% filter(dist_landcover > 0), 
       aes(x = dist_landcover, 
           color = snagyn))+
  geom_line(stat = "density", size = 1.5)+
  scale_color_discrete(name = "Coarse Woody Debris")+
  guides(color = guide_legend(override.aes = list(size=6)))+
  theme(text = element_text(size=18))+
  ggtitle("Distribution of Distance to Nearest Landcover")
  