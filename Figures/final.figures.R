source("libraries.R")
load("data/pool8.barcodes.Rda")

# 1
#############################################################################
##### Map with Barcode Points
  # get map of pool 8
  m <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), 
               zoom = 11, 
               maptype = "roadmap", 
               source = "google")

  # code for figure
  pool8.snagmap <- ggmap(m)+
      ggtitle("CWD in Pool 8")+
      xlab("Longitude")+
      ylab("Latitude")+ 
      scale_x_continuous(limits = c(-91.4, -91.1))+ #limit the x scale
      geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], #point parameters
                 aes(x = lon, y = lat, color = snag, pch = snag), 
                 size = .7, 
                 alpha = 0.8)+
      scale_color_manual(values=c("#600000", "#ff0000"))+ #point colors: light and dark red
      guides(color = guide_legend(title = "Coarse\nWoody\nDebris", #legend title for colors
                                  override.aes = list(size=8)),
             pch = guide_legend(title = "Coarse\nWoody\nDebris", #legend title for points
                                override.aes = list(size=8)))+ #legend size
      theme(text = element_text(size=20)) #all text size 20
  
  #print figure
  pool8.snagmap
  
  #save figure
  #ggsave(filename = "pool8.snagmap.ff.png", plot = pool8.snagmap, dpi = 1000)

# 2
#############################################################################
##### Map with Points and Zoom Box
  # data frame for box
  boxdf <- data.frame(x1=c(-91.292), x2=c(-91.188), y1=c(43.68), y2=c(43.757))

  # code for figure
  pool8.snagmap.box <- ggmap(m)+
    ggtitle("CWD in Pool 8")+
    xlab("Longitude")+
    ylab("Latitude")+ 
    scale_x_continuous(limits = c(-91.4, -91.1))+
    geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
                aes(x = lon, y = lat, color = snag, pch = snag), 
               size = .7, 
               alpha = 0.8)+
    scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
    guides(color = guide_legend(title = "Coarse\nWoody\nDebris", #legend title for colors
                                override.aes = list(size=8)),
           pch = guide_legend(title = "Coarse\nWoody\nDebris", #legend title for shapes
                              override.aes = list(size=8))) + 
    geom_rect(inherit.aes = FALSE, 
              data = boxdf,                  
              mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), #rectangle from boxdf
              alpha = 0, color = "black")+ #opaque black box
    theme(text = element_text(size=20)) #make all text size 20
  
  #print figure
  pool8.snagmap.box
  
  #save figure
  #ggsave(filename = "pool8.snagmap.box.ff.png", plot = pool8.snagmap.box, dpi = 1000)
  
# 3
#############################################################################
##### Zoomed in Map of Pool 8 
  # get a zoomed in map of pool8
  mm <- get_map(location = c(-91.24, mean(range(pool8.barcodes$lat))), 
                zoom = 13, 
                maptype = "roadmap", 
                source = "google")
  
  # code for figure
  pool8.snagmap.zoom <- ggmap(mm)+
    ggtitle("CWD in Pool 8")+
    xlab("Longitude")+
    ylab("Latitude")+ 
    geom_point(data = pool8.barcodes[pool8.barcodes$snag %in% c(0,1),], 
               aes(x = lon, y = lat, color = snag, pch = snag), 
               size = 1, 
               alpha = 0.8)+
    scale_color_manual(values=c("#600000", "#ff0000"))+ #light and dark red
    guides(color = guide_legend(title = "Coarse\nWoody\nDebris",
                                override.aes = list(size=8)),
           pch = guide_legend(title = "Coarse\nWoody\nDebris",
                              override.aes = list(size=8))) + #legend size
    geom_rect(inherit.aes = FALSE, 
              data=boxdf, 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              alpha = 0, 
              color="black")+
    theme(text = element_text(size=20)) #make text 20pt
  
  #print figure
  pool8.snagmap.zoom
  
  #save figure
  #ggsave(filename = "pool8.snagmap.zoom.ff.png", plot = pool8.snagmap.zoom, dpi = 1000) 
  
# 4
#############################################################################
##### Barcodes by Site Type
  # get a black and white map of pool8
  mmm <- get_map(location = c(mean(range(pool8.barcodes$lon)), mean(range(pool8.barcodes$lat))), 
                 zoom = 11, 
                 maptype = "roadmap", 
                 source = "google", 
                 color="bw")
  
  #code for figure
  pool8.sitetype <- ggmap(mmm)+
    ggtitle("Pool 8 Sampling Sites by Site Type")+
    xlab("Longitude")+
    ylab("Latitude")+ 
    scale_x_continuous(limits = c(-91.4, -91.1))+
    geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$sitetype),], 
               aes(x = lon, y = lat, color = sitetype), 
               size = 0.65, 
               alpha = 0.8, 
               pch = 20)+
    scale_color_manual(name = "Site Type", 
                       labels = c("Primary Random", 
                                  "Alternate Random",
                                  "Subjective Permanent"), 
                       values=c("#D41A1A", "#1A1AD4", "#FFC300"))+ #red, blue, yellow
    guides(color = guide_legend(override.aes = list(size=10))) +
    # add the sub perm sites on top in a bigger size
    geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$sitetype) &
                                       pool8.barcodes$sitetype == "subj.perm",], 
               size = 1.5, alpha = 0.8, pch = 20, color = "#FFC300")+
    theme(text = element_text(size=20)) #make text size 20
  
  #print figure
  pool8.sitetype
  
  #save figure
  #ggsave(filename = "pool8.sitetype.ff.png", plot = pool8.sitetype, dpi = 1000)
  
# 5 
###########################################################################
##### Sites by Wingdam Presence
  #code for figure
  wingdam.map <- ggmap(m)+
    ggtitle("Wing Dams/Dykes in Pool 8")+
    xlab("Longitude")+
    ylab("Latitude")+ 
    scale_x_continuous(limits = c(-91.4, -91.1))+
    geom_point(data = pool8.barcodes[!is.na(pool8.barcodes$wingdyke),], 
               aes(x = lon, y = lat, color = factor(wingdyke)), 
               size = 1, 
               alpha = 0.8, 
               pch = 20)+
    scale_color_manual(name = "", 
                       labels = c("No Dam at Site", 
                                  "Dam/Dyke at Site"), 
                       values=c("#FF8300", "#5D00C6"))+
    guides(color = guide_legend(override.aes = list(size=10)))+ #make legend larger
    theme(text = element_text(size=20)) #make text larger
  
  #print figure
  wingdam.map
  
  #save figure
  #ggsave(filename = "wingdam.ff.png", plot = wingdam, dpi = 1000)
  
# 6
#############################################################################
##### Map of CWD by Time
  # code for figure
  pool8.cwd.timemap <- ggmap(mmm, extent = "panel", legend = "bottomright") +
    geom_point(data = pool8.barcodes %>% filter(snag == 1), # plot the lat and lon points of all CWD
               aes(x = lon, y = lat, 
                   color = as.numeric(sdat)),  #use sdate because sdate and fdate were the same
               size = .5, 
               alpha = .8, 
               pch = 19) +
    scale_x_continuous(limits = c(-91.4, -91.1)) +
    # set the color to get brighter as the date becomes more recent
    scale_colour_gradient(low = "#400036", high = "#FF00D8", 
                          breaks = c(7500,10000,12500,15000), 
                          labels = c("07/15/90", 
                                     "05/19/97", 
                                     "03/23/04", 
                                     "01/26/11")) +
    ggtitle("Map of CWD Presence by Time") + 
    xlab("Longitude") + 
    ylab("Latitude") +
    theme(plot.title = element_text(color="#600051", #color the title to match
                                    size=14, 
                                    face="bold")) +
    labs(color = "Date of Sampling")+
    theme(text = element_text(size=20)) #make text bigger
  
  #print the figure
  pool8.cwd.timemap

  #save the figure
  #ggsave(filename = "pool8.cwd.timemap.ff.png", plot = pool8.cwd.timemap, dpi = 1000)
  
# 7
#############################################################################
##### Sideways loess curve of snag presence by latitude
  #code for figure
  LOESS <- ggplot(data = pool8.barcodes[!is.na(pool8.barcodes$snag),], 
                  aes(x = lat, y = as.numeric(as.character(snag)))) + #make snag numeric
    geom_point(size = 2, 
               alpha = 0.2, 
               pch = 20)+
    stat_smooth(method = "loess", 
                color = "darkred", #line color
                size = 1.5) + #line size
    xlab("N \n   \n  (Latitude)  \n  \n S") + #label suitable for sideways graph
    ylab("Snag Presence") +
    theme(axis.text.x = element_text(angle=270), #adjust orientation of text
          axis.title.x =  element_text(angle=270, vjust = .5),
          axis.text.y = element_text(angle=270), 
          axis.title.y =  element_text(angle=270))+
    theme(text = element_text(size=20)) #make text larger
  
  #print figure
  LOESS
  
  #save figure
  #ggsave(filename = "LOESS.ff.png", plot = LOESS, dpi = 1000)
  