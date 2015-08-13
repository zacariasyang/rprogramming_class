complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
        ## com_data <- vector(, 2)
        com_data <- data.frame()
        for(i in id){
                ## get 3 digits file name
                if(i < 10) {j <- paste("00", i, sep = "")}
                else if(i < 100) {j <- paste("0", i, sep = "")}
                else {j <- as.character(i)}
                ## set directory
                file_dir <- paste(directory, "/", j, ".csv", sep="")
                ## read data
                temp_data <- read.csv(file_dir)
                ## get out completed cases in logic vector
                com <- complete.cases(temp_data[["sulfate"]], temp_data[["nitrate"]])
                ## get number completed observations
                nob <- nrow(temp_data[com, ])
                ## assemble rows
                com_data <- rbind(com_data, data.frame(i, nob)) 
        }
        ## name the cols
        colnames(com_data) <- c("id", "nobs")
        com_data
}