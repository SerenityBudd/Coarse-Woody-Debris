firstel <- function(x){
  if(length(unique(x)) > 1){
    return(NA)
  }
  else{
    return(x[1])
  }
}
