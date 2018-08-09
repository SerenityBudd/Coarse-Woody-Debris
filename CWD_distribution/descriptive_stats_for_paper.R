# Descriptive statistics for paper
source("libraries.R")
library(DescTools)
load("data/all_reduced_7.Rda")
all_reduced <- all_reduced_7 %>% dplyr::select(-c(barcode, uniq_id, Area, pct_prm_lotic, pct_prm_lentic, num_lotic_outl, num_lentic_outl, econ, pct_terr_shore_wetf, sinuosity, NEAR_TERR_CLASS_7_N.p, NEAR_FOREST_CLASS_7_N.p, stageht.p, stratum_name, snagyn, pct_area_le100, depth.p, current.p, substrt.p, trib.p, pct_aqveg, AQUA_CODE)) %>% na.omit()
dim(all_reduced)
all <- all_reduced
colnames(all) <- c("stratum", "snag", "pool", "perimeter", "max_depth", "avg_depth", "total_volume", "shoreline_density_index", "pct_terrestrial_shore", "pct_perimeter_wetforest", "dist_to_land", "nearest_land_class", "dist_to_forest", "nearest_forest_class", "year", "wingdam", "revetment")
all$snag <- factor(as.character(all$snag))
table(all$nearest_forest_class)
#remove this factor
all$nearest_forest_class <- NULL
all$pool <- factor(all$pool, levels(all$pool)[c(2, 3, 1)] )
dim(all)

# table
with(all, table(pool, snag))

# by year
b.y <- all %>% group_by(pool, year) %>% summarize(nsnag = sum(snag == 1),
                                     nnosnag = sum(snag == 0),
                                     propsnag = sum(snag ==1)/n())

ggplot(data = b.y, aes(x = year, y = propsnag, color = pool))+
  geom_line(size = 1.5)+
  ylim(0,1)+
  theme_bw()+
  ylab("Year")+
  xlab("Proportion of points with wood")+
  ggtitle("Proportion of points with wood through time")+
  scale_color_manual(name = "Pool", values = c("darkturquoise", "firebrick2", "royalblue4"))+
  theme(text = element_text(size = 20))

# by pool
b.p <- all %>% group_by(pool) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()

chisq.test(b.p[,3:4])
# yes, these are significantly different
# pairwise comparisons?
# Pool 4 vs. Pool 8
  a <- prop.test(x = c(1151, 899), n = c(1824, 1951)) 
  # apply bonferroni correction
  a$p.value*3 #still super significant
  
# Pool 4 vs. Pool 13
  b <- prop.test(x = c(1151, 1045), n = c(1824, 1664)) 
  # apply bonferroni correction
  b$p.value*3 #not significant
  
# Pool 13 vs. Pool 8
  c <- prop.test(x = c(1045, 899), n = c(1664, 1951)) 
  # apply bonferroni correction
  c$p.value*3 #still super significant

# by stratum
b.s <- all %>% group_by(stratum) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()
cis <- BinomCI(b.s$wood, n = b.s$npoints, conf.level = 0.95)
b.s <- cbind(b.s, cis[,2:3])
chisq.test(b.s[,2:3])

    # Plot of proportions and confidence intervals by stratum
    ggplot(data = b.s, aes(x = stratum, y = propwood))+
      geom_point(size = 3)+
      geom_errorbar(aes(ymin = lwr.ci, 
                        ymax = upr.ci), 
                    width = 0,
                    size = 1.2)+
      theme_bw() +
      theme(text = element_text(size = 16))+
      ylab("Proportion of points with wood")+
      xlab("Habitat Stratum")+
      ggtitle("Wood proportion by stratum")

# by stratum and pool
b.sp <- all %>% group_by(pool, stratum) %>%
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()
chisq.test(b.sp[,3:4])

cis <- BinomCI(b.sp$wood, n = b.sp$npoints, conf.level = 0.95)
b.sp <- cbind(b.sp, cis[,2:3])

    # Plot of proportions and confidence intervals by stratum and pool
    ggplot(data = b.sp, aes(x = stratum, y = propwood, color = pool))+
      geom_point(size = 3,
                 position = position_dodge(width = 0.3))+
      geom_errorbar(aes(ymin = lwr.ci, 
                        ymax = upr.ci), 
                    width = 0,
                    size = 1.2,
                    position = position_dodge(width = 0.3)) +
      theme_bw() +
      theme(text = element_text(size = 16))+
      ylab("Proportion of points with wood")+
      xlab("Habitat Stratum")+
      ggtitle("Wood proportion by stratum")+
      scale_color_manual(name = "Pool", values = c("darkturquoise", "firebrick2", "royalblue4"))

