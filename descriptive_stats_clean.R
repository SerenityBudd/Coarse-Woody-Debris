# Descriptive Stats: LW
# Kaija Gahm
# 11 August 2018

# Source libraries
source("libraries.R")

# Load data
load("data/all_reduced_clean.Rda")
arc <- all_reduced_clean # call the data `arc` to make typing easier
arc$pool <- factor(arc$pool, levels(arc$pool)[c(2, 3, 1)]) # convert `pool` to factor and relevel
locate.nas(arc) # NA's have already been removed in the datacleaning.clean script. The column `near_forest_class` has also been removed. 

# Summarize by year and pool
b.yp <- arc %>% 
  group_by(pool, year) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag ==1)/n()) %>%
  as.data.frame()
head(b.yp)

      # Plot proportions by year and pool
      ggplot(data = b.yp, aes(x = year, y = propwood, color = pool))+
        geom_line(size = 1.5)+
        ylim(0,1)+
        theme_bw()+
        ylab("Year")+
        xlab("Proportion of points with wood")+
        ggtitle("Proportion of points with wood through time")+
        scale_color_manual(name = "Pool", 
                           values = c("darkturquoise", "firebrick2", "royalblue4"))+
        theme(text = element_text(size = 20))

# Summarize by pool
b.p <- arc %>% group_by(pool) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()
head(b.p)

      # Test for significant differences in wood proportion
      chisq.test(b.p[,3:4]) # yes, these are significantly different
    
      # Pairwise comparisons: 
      # Pool 4 vs. Pool 8
      a <- prop.test(x = c(1151, 899), n = c(1824, 1951)) # would be good to not hard code this...
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


# Summarize by stratum
b.s <- arc %>% group_by(stratum) %>% 
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()
head(b.s)

      # Calculate confidence intervals for these proportions
      cis <- BinomCI(b.s$wood, n = b.s$npoints, conf.level = 0.95)
      b.s <- cbind(b.s, cis[,2:3])
      
      # Test for significant differences
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

# Summarize by stratum and pool
b.sp <- arc %>% group_by(pool, stratum) %>%
  summarize(npoints = n(),
            wood = sum(snag == 1),
            nowood = sum(snag == 0),
            propwood = sum(snag == 1)/n()) %>%
  as.data.frame()
head(b.sp)

      # Test for significant differences
      chisq.test(b.sp[,3:4])

      # Calculate confidence intervals for the proportions
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