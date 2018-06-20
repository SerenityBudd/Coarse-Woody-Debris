source("libraries.R")

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


# PLOT THINGS

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
