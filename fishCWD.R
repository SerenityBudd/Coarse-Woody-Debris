source("libraries.R")

# import data on fish, sort the dataframes by fishcode
fishdist <- read.csv("Table_Distribution.csv")[,-c(2:5)]
fishdist <- fishdist[order(fishdist$Fishcode),] 

fishgrowth <- read.csv("Table_Growth.csv")
fishgrowth <- fishgrowth[order(fishgrowth$Fishcode),]

species <- read.csv("Table_LTRMP_Species_List.csv")
species <- species[order(species$Fishcode),] 

fishmisc <- read.csv("Table_Miscellaneous.csv")
fishmisc <- fishmisc[order(fishmisc$Fishcode),] 

fishreproduction <- read.csv("Table_Reproduction.csv")
fishreproduction <- fishreproduction[order(fishreproduction$Fishcode),] 

fishtraits <- read.csv("Table_Preference_and_Guild.csv")
fishtraits <- fishtraits[order(fishtraits$Fishcode),] 

# explore the date using dim and str
dim(fishdist)
dim(fishgrowth)
dim(species)
dim(fishmisc)
dim(fishreproduction)
dim(fishtraits)

str(species) 
str(fishmisc) 
str(fishtraits) 
str(fishreproduction) 
str(fishgrowth) 
str(fishdist)

# notice that the first col of every dataframe is the same
identical(fishdist$Fishcode, fishgrowth$Fishcode, 
          species$Fishcode, fishmisc$Fishcode, 
          fishreproduction$Fishcode, fishtraits$Fishcode)

# combine the important dataframes into fishinfo
fishinfo <- left_join(left_join(species, fishmisc, by = "Fishcode"), fishtraits, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishreproduction, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishgrowth, by = "Fishcode")
fishinfo <- left_join(fishinfo, fishdist, by = "Fishcode")


dim(fishinfo)
str(fishinfo)


# exploring the fish distribution data
table(fishdist$Mid.Range.Latitude, exclude = F)
table(fishdist$Mean.Range.Latitude, exclude = F)
table(fishdist$Ubiquity, exclude = F)

# the same fish are not in either latitude col
identical(fishdist[is.na(fishdist$Mid.Range.Latitude),], 
          fishdist[is.na(fishdist$Mean.Range.Latitude),])

# these are the fish that are NA for both Lat cols
fishdist[is.na(fishdist$Mid.Range.Latitude),1]

# grabbing the unique fishcodes from the LTRM data
FishCodes <- as.data.frame(unique(sort(fishdat$fishcode))[-1])
colnames(FishCodes) <- c("Fishcode")

# the fishcodes in both the LTRM data and fish traits
inters <- intersect(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in LTRM data not in fish traits
ltrmf <- setdiff(FishCodes[,1], fishinfo[,"Fishcode"])

# the fishcodes in the fishtraits not the LTRM data
traitsf <- setdiff(fishinfo[,"Fishcode"], FishCodes[,1])

# all the U- names are "unidentified fish type", hopefully keep some of these
#        GET RID
# UNID is generally unidentified
# I am assuming WSSN is supposed to be WDSN
# YOYF is age-0 fish
# LRVL is larval
# NFSH is no fish caught
# SCBC could be SCBS NOT confident

# remove the rows of fishcodes not in the ltrm data
fishinfo <- filter(fishinfo, Fishcode %in% inters)
identical(as.character(fishinfo[,1]), inters)

# the fish info data says the LTRM proj has not collected these
nos <- fishinfo[fishinfo$LTRMP == "N","Fishcode"]
# they have, so we will keep them
summary(fishdat[fishdat$fishcode %in% nos,"fishcode"])

# remove the rows of fishcodes that are not in the fish info data
summary(fishdat[fishdat$fishcode %in% ltrmf,"fishcode"])
#LTRMrm <- c("LRVL", "NFSH", "SCBC", "U-IL", "U-PC", "UNID", "WSSN", "YOYF")

ltrmfishdat <- filter(fishdat, !fishcode %in% ltrmf)

# explore the fishinfo data

myfunc <- function (vec) {
  table(vec, exclude = F)
}

apply(fishinfo, 2, myfunc)

# plot things

load("pool8.barcodes.Rda")
#pool8.barcodes$snag <- as.numeric(pool8.barcodes$snag)
pool8.b <- pool8.barcodes[,c("fishcode", "snag")]
pool8.b <- pool8.b[order(pool8.b$fishcode),] 
pool8.b <- pool8.b[-c(1:204),]
pool8.bb <- as.data.frame(table(pool8.b))

pool8.bb <- data.frame(pool8.bb[2:58, c("fishcode", "Freq")], 
                       pool8.bb[60:116, "Freq"])
colnames(pool8.bb) <- c("Fishcode", "nosnag", "snag")

pool8.fishtraits <- inner_join(pool8.bb, fishinfo, by = "Fishcode")


ggplot(data = filter(pool8.fishtraits,!is.na(snag) & !is.na(nosnag)), 
       aes(x = Fishcode, y = snag/(snag+nosnag), color = snag/(snag+nosnag))) +
#  scale_color_brewer(wes_palette("Rushmore1")) +
  geom_point() +
  theme(axis.text.x = element_text(size = 5, angle=60)) 
  


fishcluster <- na.omit(fishinfo[,c("Fishcode", "Wilcox.Migratory", "Wilcox.Pass.Dams", "Current.Preference", "Substrate.Preference", "Turbidity.Tolerance")])
# Wilcox.Migratory, Wilcox.Pass.Dams, Current.Preference, Substrate.Preference, Turbidity.Tolerance
fish.scaled <- scale(fishcluster[,-1])
fish.k4 <- kmeans(fish.scaled, centers=4, iter.max=100, nstart=25)
fish.k4

fish.k4.clust <- lapply(1:4, function(nc) fishcluster$Fishcode[fish.k4$cluster==nc])  
fish.k4.clust   

pairs(fishcluster[,-1], panel=function(x,y) text(x,y,fish.k4$cluster))

library(cluster)
clusplot(fish.scaled, fish.k4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(fpc)
plotcluster(fish.scaled, fish.k4$cluster)

n <- nrow(fish.scaled)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(apply(fish.scaled, 2, var))
for (i in 2:6){
  wss[i] <- sum(kmeans(fish.scaled, centers = i, iter.max=100, nstart=25)$withinss)
  plot(1:6, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
}

#seven life history attributes describing the co-evolution of traits associated with reproduction. The life history traits included ,  "longevity", "age at maturation", "total fecundity", "egg size" (mean diameter of mature, fully yolked ovarianoocytes), and "parental care" 

select(fishinfo, contains("length"))[1,]


#  c(Maximum.Literature.Length, Length.at.Maturity, Maximum.Age, Age.at.Maturity, Mean.Fecundity, Mean.Ovum.Diameter, Parental.Care)

select(fishinfo, contains("length"))
select(fishinfo, contains("age"))
select(fishinfo, contains("fecundity"))
select(fishinfo, contains("ov"))
select(fishinfo, contains("parent"))

fishcluster <- na.omit(fishinfo[,c("Fishcode", "Maximum.Literature.Length", "Length.at.Maturity", "Maximum.Age", "Age.at.Maturity", "Mean.Fecundity", "Mean.Ovum.Diameter", "Parental.Care")])
# Wilcox.Migratory, Wilcox.Pass.Dams, Current.Preference, Substrate.Preference, Turbidity.Tolerance
fish.scaled <- scale(fishcluster[,-1])
fish.k3 <- kmeans(fish.scaled, centers=3, iter.max=100, nstart=25)
fish.k3

fish.k3.clust <- lapply(1:3, function(nc) fishcluster$Fishcode[fish.k3$cluster==nc])  
fish.k3.clust   

pairs(fishcluster[,-1], panel=function(x,y) text(x,y,fish.k3$cluster))

library(cluster)
clusplot(fish.scaled, fish.k3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(fpc)
plotcluster(fish.scaled, fish.k4$cluster)

n <- nrow(fish.scaled)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(apply(fish.scaled, 2, var))
for (i in 2:6){
  wss[i] <- sum(kmeans(fish.scaled, centers = i, iter.max=100, nstart=25)$withinss)
}
plot(1:6, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")


#72      LKSG                    2159.0              889.0          80
#72               20         385079               2.90             1

#89      PDFH                    2235.0             1067.0          30
#89                9         139389               3.35             1


