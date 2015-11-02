rankhospital <- function (state, outcome, num){
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  strValidOutcome = c("heart failure","heart attack","pneumonia")
  
  if(nrow(data[data$State==state,]) == 0)
    stop ("Invalid State")
  else if (!outcome %in% strValidOutcome) 
    stop ("Invalid Outcome")
  
  subsetState <- data[data$State==state,]
  outIndex <- NULL
  
  if (outcome =="heart attack")
    ##subSetOfData <- data[data$State==state,][,11]
    outIndex <- 11
  else if (outcome =="heart failure")  
    #subSetOfData <- data[data$State==state,][,17]
    outIndex <- 17
  else if (outcome == "pneumonia") 
    #subSetOfData <- data[data$State==state,][,23]
    outIndex <- 23
  
  minVal <- min(as.numeric(subsetState[,outIndex]), na.rm = TRUE)
  minAll <- subsetState[as.numeric(subsetState[,outIndex], na.rm = TRUE) == minVal, ]
  finalSet <- minAll[!is.na(minAll[,2]),][,2]
  print (finalSet)
  ##ordInd <- order(minAll[,2], na.rm=TRUE)
  ##print (ordInd)
  #minAll[ordInd[1],][2]
  #hosp_name <- state_subset[min_index, 2]
  #return(hosp_name)
}