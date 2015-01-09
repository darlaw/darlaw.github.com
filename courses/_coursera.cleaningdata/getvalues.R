getvalues <- function(df,match, ... ){
  newlist <- vector("character")
  i=1
  for(i in 1:nrow(df)){
    column <-colnames(df)[i]
    if (grepl(match,column)) {newlist <- append(newlist, column)}
    #print(column)
    i <- i+1
    #print(i)
  }
  return(newlist)
}
