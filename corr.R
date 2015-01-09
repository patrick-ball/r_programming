corr <- function(directory, threshold=0) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
    
    ## 'threshold' is a number vector of length 1 indicating the number of 
    ## completely observed osbservations (on all variables) required to compute
    ## the correlation between nitrate and sulfate; the default value is 0
    
    ## Returns a numberic vector of correlations
    
    corvec <- numeric(0) # initialize return vector

    if (threshold == 0) {
        threshold <- 1
    }
    
    for (i in 1:332) {
        if (nchar(i) < 2) {
            filename <- paste(directory, "/00", i, ".csv", sep="")
        } else if (nchar(i) < 3) {
            filename <- paste(directory, "/0", i, ".csv", sep="")
        } else {
            filename <- paste(directory, "/", i, ".csv", sep="")
        }
        data <- read.csv(filename)
        if (sum(complete.cases(data)) >= threshold) {
            completedata <- data[complete.cases(data),]
            sncor <- cor(completedata["sulfate"], completedata["nitrate"])
            corvec <- c(corvec, sncor)
        }
    }
    corvec
}