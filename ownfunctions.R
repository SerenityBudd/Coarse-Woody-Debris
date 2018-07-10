# Useful functions

#First Element: takes the first element if there's only one unique value in the column, returns NA otherwise. 
firstel <- function(x){
  if(length(unique(x)) > 1){
    return(NA)
  }
  else{
    return(x[1])
  }
}


#Count NA's: produces a data frame with column names and number of na's
locate.nas <- function(df){
  na.df <- data.frame(
    colname = names(df),
    nas = NA
  )
  
  for(i in 1:ncol(df)){
    na.df$nas[i] <- sum(is.na(df[,i]))
  }
  return(na.df)
}


#Notin: define %notin% operator
`%notin%` <- Negate(`%in%`) 