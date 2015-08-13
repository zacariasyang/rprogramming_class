corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
        ## create data frame of id and nobs across all observation points
        df <- complete(directory)
        ## get qualified points
        df <- df[df$nobs > threshold, ]
        # get qualified ID
        ids <- df[,"id"]
        cr <- vector('numeric')
        for(i in ids){
                ## get 3 digits file name
                if(i < 10) {j <- paste("00", i, sep = "")}
                else if(i < 100) {j <- paste("0", i, sep = "")}
                else {j <- as.character(i) }
                ## set directory
                file_dir <- paste(directory, "/", j, ".csv", sep="")
                ## read data
                temp_data <- read.csv(file_dir)
                ## get out completed cases in logic vector
                com <- complete.cases(temp_data[["sulfate"]], temp_data[["nitrate"]])
                ## get completed cases data
                com_data <- temp_data[com, ]
                cr[length(cr)+1] <- cor(com_data$sulfate, com_data$nitrate)
        }
        cr
}