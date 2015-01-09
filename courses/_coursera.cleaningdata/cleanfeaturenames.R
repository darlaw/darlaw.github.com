cleanfeaturenames <- function(list, ... ){
  i=1
  for(i in 1:nrow(list)){
    value <- list[i,2]
    if (grepl("[\\(][\\)]",x=value)){
      value<-gsub("[\\(][\\)]",replacement="",x=value) #replace open paren
    }  else if (grepl("[\\(][^\\)]",x=value)){
      value<-gsub("[\\(]",replacement=".",x=value) #replace close paren
      value<-gsub("[\\)]",replacement="",x=value) #replace close paren
    } 
    if (grepl("[\\,]",x=value)){
      value<-gsub("[\\,]",replacement=".",x=value) #replace comma 
    }
    value <- make.names(value)
    list[i,2] <- value
    #print(list[i,2])
    i <- i+1
    #print(i)
  }
    return(list)
}