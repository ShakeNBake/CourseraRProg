corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        completeobs <- complete(directory)
        
        abovethreshold <- completeobs$nobs > threshold
        
        id <- completeobs$id[abovethreshold]
        
        sulfates <- as.numeric()
        nitrates <- as.numeric()

        i <- 1L
        charid <- as.character()
        cr <- as.numeric()
        
        while(i <= length(id)) {
                # Precede IDs with zeros to match filename convention
                if(nchar(id[i]) == 1) {
                        charid[i] <- paste0("00", id[i])
                } else if (nchar(id[i]) == 2) {
                        charid[i] <- paste0("0", id[i])
                } else {charid[i] <- id[i]}
                filename <- paste0(charid[i], ".csv")
                
                # Read CSV data from a monitor
                monitor <- read.csv(paste(directory, filename, sep = "/"))
                
                # Find complete observations
                good <- complete.cases(monitor$sulfate, monitor$nitrate)
                
                # Append sulfates and nitrates vectors
                sulfates <- monitor$sulfate[good]
                nitrates <- monitor$nitrate[good]
                
                correlation <- cor(sulfates, nitrates)
                cr <- c(cr, correlation)

                #Proceed to the next monitor in the 'id' vector
                i <- i + 1
        }
        
        # Output vector of Correlations 
        cr
}
