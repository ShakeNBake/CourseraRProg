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
        
        # Initialize counter
        i <- 1L
        
        # Create an empty data frame to store results
        comp <- data.frame()
        charid <- as.character()
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
                
                # Count complete observations
                good <- complete.cases(monitor$sulfate, monitor$nitrate)
                countcomplete <- sum(good)
                
                # Append the data frame containing results
                permonitor <- c(id[i], countcomplete)
                comp <- rbind(comp, permonitor)
                
                #Proceed to the next monitor in the 'id' vector
                i <- i + 1
        }
        # Rename columns in the output data frame
        colnames(comp) <- c("id", "nobs")
        
        # Return final data frame
        comp
}
