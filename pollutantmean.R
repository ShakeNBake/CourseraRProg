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
        i <- 1L
        polvec <- as.numeric()
        polvec <- while(i <= length(id)) {
                if(nchar(id[i]) == 1) {
                        id[i] <- paste0("00", id[i])
                } else if (nchar(id[i]) == 2) {
                        id[i] <- paste0("0", id[i])
                }
                filename <- paste0(id[i], ".csv")
                monitor <- read.csv(paste(directory, filename, sep = "/"))
                polvec <- c(polvec,monitor$pollutant)
                i <- i + 1
        }
#        retmean <- mean(polvec, na.rm = TRUE)
        polvec
}
