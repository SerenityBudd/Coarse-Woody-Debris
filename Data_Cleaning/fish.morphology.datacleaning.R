source("libraries.R")
load("data/fishinfo.Rda")

# name the rfishbase dataframe
fb <- fishbase

# create a scientific name vector 
sciname <- paste(fb$Genus, fb$Species)

# index the fish
index <- fishinfo$Scientific.Name.Current %in% sciname
fishcorrect <- fishinfo[index,]
dim(fishcorrect)
#132 species left

morpho <- morphometrics(species_list = fishcorrect$Scientific.Name.Current)
#28 or more
length(unique(morpho$sciname))

#end up with 118 species

badfish <- fishinfo[!index,]
unique(badfish$Scientific.Name.Current)

str(morpho)

morpho_new <- dplyr::select(morpho, sciname:CA)

morpho_grouped <- morpho_new %>%
  group_by(sciname) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

morpho_grouped_complete <- morpho_grouped[complete.cases(morpho_grouped),]
dim(morpho_grouped_complete)
# 110 fish left

fishclustercomplete <- morpho_grouped_complete
save(fishclustercomplete, file = "data/fishclustercomplete.Rda")
