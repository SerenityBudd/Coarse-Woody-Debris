#Count NA's
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
