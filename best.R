best <- function(state, outcome) {
  outcome2 <- data.frame()
  outcome2 <- read.csv("C:/Users/sakoya00/Documents/outcome-of-care-measures.csv")
  if(!state %in% state.abb){
    stop("invalid state")
  }
  else if(!(identical(outcome, "heart attack")| identical(outcome, "heart failure") | identical (outcome, "pneumonia"))){
    stop("invalid outcome")
  }
  else{
    staterows <- subset(outcome2, outcome2$State == state)
    lowhospital <- c()

    
    n <- integer()
    if(identical(outcome, "heart attack"))
      
      n <- 11
      
    
    
    if(identical(outcome, "heart failure"))
      
      n <- 17
      
    
    
    if(identical(outcome, "pneumonia"))
      
      n <- 23
      
    
    hospitalrow <- integer()
    #remember to convert to numeric or min will return wrong value
    hospitalrow <- which(as.numeric(staterows[ ,n]) == min(as.numeric(staterows [,n]), na.rm= TRUE)) 
    
    min <- numeric()
    min <- as.numeric(staterows [hospitalrow, n])
    
    
  }
  lowhospital <- append (lowhospital, staterows [hospitalrow, 2])
  
  for(i in nrow(staterows)){
    
    if(identical(as.numeric(staterows[i, n]), min) & !identical(staterows[hospitalrow,2], staterows[i,2]))
      
      lowhospital <- append (lowhospital, staterows [i,2])
    
  }
  return(min(lowhospital))
}

