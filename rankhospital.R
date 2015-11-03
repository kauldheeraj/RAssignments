rankhospital <- function (state, outcome, num="best"){
  data <- read.csv("/home/dheeraj/Desktop/Coursera/outcome-of-care-measures.csv", colClasses="character")
  strValidOutcome = c("heart failure","heart attack","pneumonia")
  
  if(nrow(data[data$State==state,]) == 0)
    stop ("Invalid State")
  else if (!outcome %in% strValidOutcome) 
    stop ("Invalid Outcome")
  
  subsetState <- data[data$State==state,]
  outIndex <- NULL
  
  if (outcome =="heart attack")
    outIndex <- 11
  else if (outcome =="heart failure")  
    outIndex <-17
  else if (outcome == "pneumonia") 
    outIndex <- 23
  
  subsetState <- subsetState[order(subsetState[,outIndex], subsetState[,2], na.last = NA),]
  subsetState <- subsetState[!is.na(as.numeric(subsetState[,outIndex])),]
  ##print (subsetState[,2])
  ##print (subsetState[,outIndex])
  if (num == "best")
    subsetState[1,2]
  else if (num == "worst")
    subsetState[nrow(subsetState),2]
  else 
    subsetState[num,2]
  

}