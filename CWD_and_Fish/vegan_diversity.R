source("libraries.R")
load("data/funcdiv4.8.13.Rda")

################################################
## Shannon Weaver Diversity

shannon_div <- function(Pool) {
  ## filter the dataframe by pool
  funcy <- funcdiv4.8.13 %>% 
    filter(pool %in% Pool) 
  ## create a dataframe specifying the number of fish per species per barcode
  xxx <- funcy %>%
    group_by(barcode) %>% count(Fishcode)
  colnames(xxx)[3] <- "Num_Fish_per_Species"
  
  ## spread the dataframe so each col is a fishcode
  aaa <- spread(data = xxx, value = Num_Fish_per_Species, Fishcode) 
  ## replace the NAs with zero
  aaa[is.na(aaa)] <- 0
  ## make the tibble a dataframe
  aaa <- data.frame(aaa)
  
  ## make a dataframe that includes barcode/snag
  dt.plot <- left_join(aaa, select(funcy, c(barcode, snag)), by = "barcode") %>% distinct 
  dt.plot <- dt.plot[complete.cases(dt.plot),]
  
  ## make a dataframe to do analysis on, leaves out barcodes and snag out
  bbb <- select(dt.plot, -c(barcode, snag))
  
  ## use 'vegan' to calculate the shannon diversity index for each barcode
  div_shannon <- diversity(x = bbb, index = "shannon", base = 2)
  
  ## create a column in the dataframe for the shannon div index
  dt.plot$div_shan <- div_shannon
  
  ## run a t-test on shannon div and snag
  print(with(dt.plot, t.test(div_shan~snag, alternative = "less")))
  
  ## plot the densities of the vectors above
  ggplot(data = dt.plot, aes(div_shan)) + 
    geom_density(aes(fill = snag), alpha = .3) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    scale_fill_manual(values = c("blue", "red"), name = "Snag") + 
    geom_vline(xintercept = c(1.5, 3.5), colour = "black") + 
    xlab("Shannon–Weaver Diversity Index") +
    ylab("Density") +
    ggtitle(paste("Density Plot of Shannon-Weaver Index at Pool", Pool, "Sites"))
}


shannon_div(Pool = "08")
shannon_div(Pool = "04")
shannon_div(Pool = "13")
shannon_div(Pool = c("04","08","13"))


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


################################################

geomSeries <- function(base, max) {
  base^(0:floor(log(max, base)))
}

geomSeries(base=2, max=2000)
################################################


