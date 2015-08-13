pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
        col_sum <- 0
        row_num <- 0
        for(i in id){
                ## get 3 digits file name
                if(i < 10) {j <- paste("00", i, sep = "")}
                else if(i < 100) {j <- paste("0", i, sep = "")}
                else {j <- i}
                ## set directory
                file_dir <- paste(directory, "/", j, ".csv", sep="")
                ## read data
                temp_data <- read.csv(file_dir)
                ## extract pollutant col
                temp_col <- temp_data[[pollutant]]
                ## summing up non-NA numbers and observations
                col_sum <- col_sum + sum(temp_col[!is.na(temp_col)])
                row_num <- row_num + length(temp_col[!is.na(temp_col)])
        }
        col_sum / row_num
        
}