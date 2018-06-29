source("libraries.R")
load("data/fishinfo.Rda")

# explore the fishinfo data
for (i in 1:ncol(fishinfo)) {
  print(colnames(fishinfo)[i]) 
  print(summary(fishinfo[,i]))
}

#Quicky determine how many NA's are in each column of the data frame. 
nacount <- function(x){
  na.df <- data.frame(name = names(x), nas = NA)
  for(i in 1:ncol(x)){
    na.df$nas[i] <- sum(is.na(x[,i]))
  }
  return(na.df)
}

# count NA's
nacount(fishinfo)

# remove the columns with too many NAs
fishinfo <- select(fishinfo, -c(Animal, Wilcox.Ucrit, Freshwater.Marine, Maximum.LTRMP.Length, Substock:Trophy))

# selects only the columns that are numeric
fishcluster <- select(fishinfo, c(Fishcode, Exploit.Rank:Wilcox.Pass.Dams, Conservation.Status:Trophic.Guild, Water.Column.Preference:Egg.Bouyancy,Maximum.Fecundity:Mean.Incubation,Larval.Growth:Ubiquity))

# quickly see summaries for the new df fishcluster
for (i in 1:ncol(fishcluster)) {
  print(colnames(fishcluster)[i]) 
  print(summary(fishcluster[,i]))
}

# count NA's
nacount(fishcluster)

# selects the columns that are important to cluster analysis
# method from paper
fishcluster1 <- fishinfo[,c("Common.Name", "Maximum.Literature.Length", "Length.at.Maturity", "Maximum.Age", "Age.at.Maturity", "Mean.Fecundity", "Mean.Ovum.Diameter", "Parental.Care")]

# removing repeat variables
fishcluster2 <- select(fishcluster, -c(Range.Ovum.Diameter,Adult.Trophic.Level, Maximum.Fecundity, Juvenile.Cutoff))

# remove variables with relatively more NAs
fishcluster3 <- select(fishcluster, -c(Range.Ovum.Diameter,Adult.Trophic.Level, Maximum.Fecundity, Juvenile.Cutoff,Egg.Bouyancy,Mean.Ovum.Diameter,Mean.Incubation,Larval.Growth))

# another variation
fishcluster4 <- select(fishinfo, c(Common.Name, Swim.Factor, Shape.Factor,Maximum.Literature.Length, Trophic.Guild, Length.at.Maturity, Maximum.Age, Age.at.Maturity, Mean.Fecundity, Mean.Ovum.Diameter, Parental.Care, R.Guild1:F.Guild3))

# make a dataframe of fishcluster with all the NAs removed, *** NOTE which "fishcluster" is used
fishclustercomplete <- fishcluster4[complete.cases(fishcluster4),]
#save(fishclustercomplete, file = "data/fishclustercomplete.Rda")
