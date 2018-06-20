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
# order pool8.b by fishcode
pool8.b <- pool8.b[order(pool8.b$fishcode),] 
# remove rows with missing fishcodes
pool8.b <- pool8.b[-c(1:204),]
# make the table a dataframe so we can work with it
pool8.bb <- as.data.frame(table(pool8.b))

pool8.bb <- data.frame(pool8.bb[2:58, c("fishcode", "Freq")], 
                       pool8.bb[60:116, "Freq"])
colnames(pool8.bb) <- c("Fishcode", "nosnag", "snag")

pool8.fishtraits <- inner_join(pool8.bb, fishinfo, by = "Fishcode")

#plot snag/(snag + nosnag) by fishcode
ggplot(data = filter(pool8.fishtraits,!is.na(snag) & !is.na(nosnag)), 
       aes(x = Fishcode, y = snag/(snag+nosnag), color = snag/(snag+nosnag))) +
  geom_point() +
  theme(axis.text.x = element_text(size = 5, angle=60)) 
  
# use cluster analysis on the fish data 
# c("Fishcode", "Wilcox.Migratory", "Wilcox.Pass.Dams", "Current.Preference", "Substrate.Preference", "Turbidity.Tolerance")
# c("Fishcode", Maximum.Literature.Length, Length.at.Maturity, Maximum.Age, Age.at.Maturity, Mean.Fecundity, Mean.Ovum.Diameter, Parental.Care)

select(fishinfo, contains("length"))
select(fishinfo, contains("age"))
select(fishinfo, contains("fecundity"))
select(fishinfo, contains("ov"))
select(fishinfo, contains("parent"))

fishcluster <- na.omit(fishinfo[,c("Fishcode", "Maximum.Literature.Length", "Length.at.Maturity", "Maximum.Age", "Age.at.Maturity", "Mean.Fecundity", "Mean.Ovum.Diameter", "Parental.Care")])

# using Partitioning Methods
fish.scaled <- scale(fishcluster[,-1])

# partitioning into 3 groups
fish.k3 <- kmeans(fish.scaled, centers=3, iter.max=100, nstart=25)
fish.k3

# these are the 3 groups
fish.k3.clust <- lapply(1:3, function(nc) fishcluster$Fishcode[fish.k3$cluster==nc])  
fish.k3.clust   

# plot the scatterplot matrix
pairs(fishcluster[,-1], panel=function(x,y) text(x,y,fish.k3$cluster))

# Cluster Plot against first 2 principal components
clusplot(fish.scaled, fish.k3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

plotcluster(fish.scaled, fish.k3$cluster)

# look for the "elbow" to see the best number of groups
n <- nrow(fish.scaled)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(apply(fish.scaled, 2, var))
for (i in 2:6){
  wss[i] <- sum(kmeans(fish.scaled, centers = i, iter.max=100, nstart=25)$withinss)
}
plot(1:6, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")

# using Hierarchical Clustering
fish.dist <- dist(scale(fishcluster[,-1]))

# single linkage
fish.single.link <- hclust(fish.dist, method='single')
plot(fish.single.link, labels=fishcluster$Fishcode)

# complete linkage
fish.complete.link <- hclust(fish.dist, method='complete')
plot(fish.complete.link, labels=fishcluster$Fishcode)

# looking at 2 groups
cut.2 <- cutree(fish.complete.link, k=2)

pairs(fishcluster[,-1], panel=function(x,y) text(x,y,cut.2))

food.2.clust <- lapply(1:2, function(nc) fishcluster$Fishcode[cut.2==nc])  
food.2.clust

# looking at 3 groups
cut.3 <- cutree(fish.complete.link, k=3)

pairs(fishcluster[,-1], panel=function(x,y) text(x,y,cut.3))

food.3.clust <- lapply(1:3, function(nc) fishcluster$Fishcode[cut.3==nc])  
food.3.clust

# principal components
fish.pc <- princomp(fishcluster[,-1],cor=T)

# Setting up the colors for the 3 clusters on the plot:
my.color.vector <- rep("red", times=nrow(fishcluster))
my.color.vector[cut.3==2] <- "blue"
my.color.vector[cut.3==3] <- "orange"

# Plotting the PC scores:
plot(fish.pc$scores[,1], fish.pc$scores[,2], ylim=range(fish.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(fish.pc$scores[,1], fish.pc$scores[,2], labels=fishcluster$Fishcode, cex=0.7, lwd=2,
     col=my.color.vector)

# the fish that were outliers were #72 - LKSG and #89 - PDFH

