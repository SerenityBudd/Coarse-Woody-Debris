source("libraries.R")
load("data/funcdiv4.8.13.Rda")
#######################################################

head(funcdiv4.8.13)


Cen.funcdiv <- filter(funcdiv4.8.13, Family.Name == "Centrarchidae") %>%
  droplevels()

dim(Cen.funcdiv)
#save(Cen.funcdiv, file = "data/Cen.funcdiv.Rda")

with(Cen.funcdiv, table(snag))
with(Cen.funcdiv, table(stratum_name,snag))
with(Cen.funcdiv, table(pool, snag))
with(Cen.funcdiv, table(Trophic.Guild))
with(Cen.funcdiv, table(Common.Name))


#######################################################


cen_rich <- function(Pool) {
  ## filter the dataframe by pool
  funcy <- funcdiv4.8.13 %>% 
    filter(pool == Pool) 
  ## create a dataframe specifying the number of fish per species per barcode
  xxx <- funcy %>%
    group_by(barcode) %>% count(Fishcode)
  colnames(xxx)[3] <- "Num_Fish_per_Species"
  
  ## create a dataframe specifying the number of unique species per barcode
  yyy <- xxx %>% group_by(barcode) %>% count(barcode)
  colnames(yyy)[2] <- "Num_Species"

  ## create a richness dataframe to do a t test on, combinging snag and yyy
  dt.richness.t <- left_join(yyy, select(funcy, c(barcode, snag)), by = "barcode") %>% distinct
  
  ## there is a signigficant diff in richness between sites with and without CWD
  print(with(dt.richness.t, t.test(Num_Species~snag, alternative = "less"))) 
 
  with(dt.richness.t, boxplot(Num_Species~snag))
  
  ## plot the densities of the vectors above
  ggplot(data = dt.richness.t, aes(Num_Species)) + 
    geom_density(aes(fill = snag), alpha = .3) +
    scale_fill_manual(values = c("blue", "red")) + 
    xlab("Centrarchidae Richness") +
    ylab("Density") +
    ggtitle(paste("Density Plot of Centrarchidae Richness at Pool", Pool, "Sites"))
}

cen_rich("08")
cen_rich("04")
cen_rich("13")




################################################
## Rank abundance curve - trophic guilds

RACplot.cen <- function(Pool, Snag, Stratum) {
  ## create a dataframe filtered by pool, snag, and stratum, trophic guild
  funcy <- 
    Cen.funcdiv %>%
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
    ## log transform the scale
    scale_y_continuous(trans='log10', breaks= c(10^c(0:5))) +
    scale_color_manual(values=c(brewer.pal(n = 6, name = "Set1"))) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    xlab("Common Name") +
    ylab("Abundance (Log10 Scale)") +
    ggtitle(paste("Centrarchidae Abundance for Pool", Pool, Stratum, "Sites",
                  ifelse(Snag == "No", paste("Where Snag Was Not Present"),  paste("Where Snag Was Present"))))
}

RACplot.cen("08", "Yes", "Backwater, Contiguous Shoreline")
RACplot.cen("08", "No", "Backwater, Contiguous Shoreline")
RACplot.cen("04", "Yes", "Backwater, Contiguous Shoreline")
RACplot.cen("04", "No", "Backwater, Contiguous Shoreline")
RACplot.cen("13", "Yes", "Backwater, Contiguous Shoreline")
RACplot.cen("13", "No", "Backwater, Contiguous Shoreline")

RACplot.cen("08", "Yes", "Impounded, Shoreline")
RACplot.cen("08", "No", "Impounded, Shoreline")
RACplot.cen("08", "Yes", "Main Channel Border, Unstructured")
RACplot.cen("08", "No", "Main Channel Border, Unstructured")
RACplot.cen("08", "Yes", "Main Channel Border, Wing Dam Area")
RACplot.cen("08", "No", "Main Channel Border, Wing Dam Area")
RACplot.cen("08", "Yes", "Side Channel Border")
RACplot.cen("08", "No", "Side Channel Border")

