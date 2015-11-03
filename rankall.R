rankall <- function (outcome, num="best"){
  data <- read.csv("/home/dheeraj/Desktop/Coursera/outcome-of-care-measures.csv")
  strValidOutcome = c("heart failure","heart attack","pneumonia")
  
  outIndex <- NULL
  dfOut <- data.frame(hospital=factor(), state=factor())
  if (outcome =="heart attack")
    outIndex <- 11
  else if (outcome =="heart failure")  
    outIndex <-17
  else if (outcome == "pneumonia") 
    outIndex <- 23
  data[, outIndex] <- suppressWarnings(as.numeric(levels(data[, outIndex])[data[, outIndex]]))
  #data[, 2] <- as.character(data[, 2])
  states <- levels(data[,7])
   
  for (i in 1:length(states)){
    subsetState <- data[data$State==states[i],]
    subsetState <- subsetState[order(subsetState[,outIndex], subsetState[,2], na.last = NA),]
    subsetState <- subsetState[!subsetState[,outIndex] == "Not Available",]
    if (num == "best")
      varHospital <- subsetState[1,2]
    else if (num == "worst")
      varHospital <- subsetState[nrow(subsetState),2]
    else 
      varHospital <- subsetState[num,2]
      
    dfOut <- rbind(dfOut, data.frame(hospital = varHospital, state = states[i] ))
  }
  dfOut

  
}