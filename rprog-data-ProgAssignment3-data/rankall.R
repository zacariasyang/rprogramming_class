rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (outcome == "heart attack") colnumber <- 11
  else if (outcome == "heart failure") colnumber <- 17
  else if (outcome == "pneumonia") colnumber <- 23
  else stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate with the given rank
  fct <- factor(data$State)
  fctlv <- levels(fct)
  df <- data.frame()
  for (i in fctlv) {
    data1 <- data[data[, 7] == i, ] # subset the State
    data2 <- data1[data1[, colnumber] != "Not Available", ] #subset the non-NAs
    data2[, colnumber] <- as.numeric(data2[, colnumber]) # make outcome percentages cols numeric
    data3 <- data2[order(data2[,colnumber], data2[, 2]), ] # sort by outcome and hospital name
    data4 <- data3[1, c(2, 7)]
    row.names(data4) <- i
     if (is.character(num) && num == "best") {
       data4 <- data3[1, c(2, 7)]
       row.names(data4) <- i
       df <- rbind(df, data4)
     }
     else if (is.character(num) && num == "worst") {
       data4 <- data3[nrow(data3), c(2, 7)]
       row.names(data4) <- i
       df <- rbind(df, data4)
     }
     else if (is.numeric(num) && num > nrow(data3)) {
       data4 <- data3[num, c(2, 7)]
       data4[1, 1] <- NA
       data4[1, 2] <- i
       row.names(data4) <- i
       df <- rbind(df, data4)
     }
     else {
       data4 <- data3[num, c(2, 7)]
       row.names(data4) <- i
       df <- rbind(df, data4)
    }
  }
  colnames(df) <- c("hospital", "state")
  df
}