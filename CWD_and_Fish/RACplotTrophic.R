source("libraries.R")
load("data/funcdiv4.8.13.Rda")

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
