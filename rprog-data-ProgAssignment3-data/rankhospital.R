rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data_s <- split(data, data$State) # split into list by State
  data_s_sub <- data_s[[state]]
  
  ## Check that state and outcome are valid
  if (sum(data$State == state) == 0) stop("invalid state")
  if (outcome == "heart attack") colnumber <- 11
  else if (outcome == "heart failure") colnumber <- 17
  else if (outcome == "pneumonia") colnumber <- 23
  else stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate with the given rank
  
  o_data <- data_s_sub[data_s_sub[, colnumber] != "Not Available", ]
  o_data[, colnumber] <- as.numeric(o_data[, colnumber])
  o_data_narm <- o_data[order(o_data[, colnumber], o_data[,2]), ]
  if (is.character(num) && num == "best") return(o_data_narm[1, 2])
  else if (is.character(num) && num == "worst") return(o_data_narm[nrow(o_data_narm),2])
  else if (is.numeric(num) && num > nrow(o_data_narm)) return(NA)
  else return(o_data_narm[num, 2])
}