source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Rank abundance curve - trophic guilds

RACplot <- function(Pool, Snag, Stratum) {
  ## create a dataframe filtered by pool, snag, and stratum, trophic guild
  funcy <- 
    funcdiv4.8.13 %>%
    filter(pool == Pool
           & snag == Snag
           & stratum_name == Stratum 
           & !is.na(Trophic.Guild))
  
  ## create a dataframe to plot
  xplot <- 
    ## subset the dataframe into communities by stratum and snag
    funcy %>% 
    #group_by(stratum_name, snag) %>% 
    ## fish the species abundance at each community
    count(Fishcode) %>%
    ## add in the variables we need to plot
    left_join(., select(funcy, c(Fishcode, Trophic.Guild, Common.Name)), by = "Fishcode") %>%
    distinct()
  
  ## plot abundance vs common name, from most to least abundant
  ggplot(data = xplot, aes(x = reorder(Common.Name, -n), n, group = "group")) +
    ## color by trophic guild
    geom_point(stat = "identity", aes(color = Trophic.Guild), size = 3) +
    geom_segment(aes(x = 1, xend = length(Common.Name), y = max(n), yend = min(n)), data = xplot) +
    ## log transform the scale 
    scale_y_continuous(trans='log10', breaks= c(10^c(0:5))) +
    scale_color_manual(values=c(brewer.pal(n = 6, name = "Set1"))) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Common Name") +
    ylab("Abundance (Log10 Scale)") +
    ggtitle(paste("Species Abundance for Pool", Pool, Stratum, "Sites",
                  ifelse(Snag == "No", paste("Where Snag Was Not Present"),  paste("Where Snag Was Present"))))
}

RACplot("08", "Yes", "Backwater, Contiguous Shoreline")
RACplot("08", "No", "Backwater, Contiguous Shoreline")
RACplot("04", "Yes", "Backwater, Contiguous Shoreline")
RACplot("04", "No", "Backwater, Contiguous Shoreline")
RACplot("13", "Yes", "Backwater, Contiguous Shoreline")
RACplot("13", "No", "Backwater, Contiguous Shoreline")

RACplot("08", "Yes", "Impounded, Shoreline")
RACplot("08", "No", "Impounded, Shoreline")
RACplot("08", "Yes", "Main Channel Border, Unstructured")
RACplot("08", "No", "Main Channel Border, Unstructured")
RACplot("08", "Yes", "Main Channel Border, Wing Dam Area")
RACplot("08", "No", "Main Channel Border, Wing Dam Area")
RACplot("08", "Yes", "Side Channel Border")
RACplot("08", "No", "Side Channel Border")


################################################
## Rank abundance curve - Native Fish

RACplotNative <- function(Pool, Snag, Stratum) {
  ## create a dataframe filtered by pool, snag, and stratum, native
  funcy <- 
    funcdiv4.8.13 %>%
    filter(pool == Pool
           & snag == Snag
           & stratum_name == Stratum 
           & !is.na(Trophic.Guild))
  
  ## create a dataframe to plot
  xplot <- 
    ## subset the dataframe into communities by stratum and snag
    funcy %>% group_by(stratum_name, snag) %>% 
    ## fish the species abundance at each community
    count(Fishcode) %>%
    ## add in the variables we need to plot
    left_join(., select(funcy, c(Fishcode, Native, Common.Name)), by = "Fishcode") %>%
    distinct()
  
  ## plot abundance vs common name, from most to least abundant
  ggplot(data = xplot, aes(x = reorder(Common.Name, -n), n, group = "group")) +
    ## color by native
    geom_point(stat = "identity", aes(color = Native), size = 3) +
    ## log transform the scale 
    scale_y_continuous(trans='log10', breaks= c(10^c(0:5))) +
    scale_color_manual(values=c("blue", "red")) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Common Name") +
    ylab("Abundance (Log10 Scale)") +
    ggtitle(paste("Species Abundance for Pool", Pool, Stratum, "Sites",
                  ifelse(Snag == "No", paste("Where Snag Was Not Present"),  paste("Where Snag Was Present"))))
}

RACplotNative("08", "Yes", "Main Channel Border, Unstructured")
RACplotNative("08", "No", "Main Channel Border, Unstructured")
RACplotNative("08", "Yes", "Backwater, Contiguous Shoreline")
RACplotNative("08", "No", "Backwater, Contiguous Shoreline")

RACplotNative("08", "Yes", "Impounded, Shoreline")
RACplotNative("08", "No", "Impounded, Shoreline")
RACplotNative("08", "Yes", "Main Channel Border, Unstructured")
RACplotNative("08", "No", "Main Channel Border, Unstructured")
RACplotNative("08", "Yes", "Main Channel Border, Wing Dam Area")
RACplotNative("08", "No", "Main Channel Border, Wing Dam Area")
RACplotNative("08", "Yes", "Side Channel Border")
RACplotNative("08", "No", "Side Channel Border")

################################################
RACplotTrophic <- function(Pool) {
  
  funcy <- 
    funcdiv4.8.13 %>%
    filter(pool == Pool
           & snag == "Yes"
           & !is.na(Trophic.Guild))
  
  xplot <- 
    funcy %>% group_by(snag) %>% 
    count(Trophic.Guild) %>%
    distinct()
  
  funcy1 <- 
    funcdiv4.8.13 %>%
    filter(pool == Pool
           & snag == "No"
           & !is.na(Trophic.Guild))
  
  xplot1 <- 
    funcy1 %>% group_by(snag) %>% 
    count(Trophic.Guild) %>%
    distinct()
  
  xplot2 <- bind_rows(xplot, xplot1)
  
  
  ggplot(data = xplot2, aes(x = reorder(Trophic.Guild, -n), n, group = "group")) +
    geom_point(aes(shape = snag, fill = Trophic.Guild), size = 5) +
    scale_shape_manual(values = c(21, 24)) +
    scale_fill_manual(values=c("#f1a340", "#B640FF","#a6cee3", "#e41a1c", "#ffffb3", "#4daf4a"),
                      name="Trophic Guild",
                      breaks=c("Herbivore","Omnivore","General Invertivore","Benthic Invertivore" ,"Piscivore","Planktivore"),
                      labels=c("Herbivore","Omnivore","General Invertivore","Benthic Invertivore" ,"Piscivore","Planktivore")) +
    guides(fill = guide_legend(override.aes=list(shape = 21))) +
    scale_y_continuous(trans='log10', breaks= c(10^c(0:5))) +
    theme_bw() +
    theme(axis.text.x=element_blank()) +
    xlab("") +
    ylab("Abundance (Log10 Scale)") +
    ggtitle(paste("Species Abundance for Pool", Pool, "Sites"))
}


RACplotTrophic("08")
RACplotTrophic("04")
RACplotTrophic("13")


RACplotTrophic("08", "Yes", "Impounded, Shoreline")
RACplotTrophic("08", "No", "Impounded, Shoreline")
RACplotTrophic("08", "Yes", "Main Channel Border, Unstructured")
RACplotTrophic("08", "No", "Main Channel Border, Unstructured")
RACplotTrophic("08", "Yes", "Main Channel Border, Wing Dam Area")
RACplotTrophic("08", "No", "Main Channel Border, Wing Dam Area")
RACplotTrophic("08", "Yes", "Side Channel Border")
RACplotTrophic("08", "No", "Side Channel Border")

################################################
## Abundance plot 

ABUNDplot <- function(Pool, Snag, Stratum) {
  ## create a dataframe filtered by pool, snag, and stratum, trophic guild
  funcy <- 
    funcdiv4.8.13 %>%
    filter(pool == Pool
           & snag == Snag
           & stratum_name == Stratum 
           & !is.na(Trophic.Guild))
  
  ## create a dataframe to plot
  xplot <- 
    ## subset the dataframe into communities by stratum and snag
    funcy %>% group_by(stratum_name, snag) %>% 
    ## fish the species abundance at each community
    count(Fishcode) %>%
    ## add in the variables we need to plot
    left_join(., select(funcy, c(Fishcode, Trophic.Guild, Common.Name)), by = "Fishcode") %>%
    distinct()
  
  ## plot abundance vs common name, from most to least abundant
  ggplot(data = xplot, aes(x = reorder(Common.Name, -n), n, group = "group")) +
    ## color by trophic guild
    geom_point(stat = "identity", aes(color = Trophic.Guild), size = 3) +
    scale_color_manual(values=c(brewer.pal(n = 6, name = "Set1"))) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Common Name") +
    ylab("Abundance") +
    ggtitle(paste("Species Abundance for Pool", Pool, Stratum, "Sites",
                  ifelse(Snag == "No", paste("Where Snag Was Not Present"),  paste("Where Snag Was Present"))))
}

ABUNDplot("08", "No", "Backwater, Contiguous Shoreline")


