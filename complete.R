complete <- function(directory, id=1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
        
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## Returns a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID and 'nobs' is the # of complete cases

    d <- data.frame("id" = id, "nobs" = 0)
    row = 1
    for (i in id) {
        if (nchar(i) < 2) {
            filename <- paste(directory, "/00", i, ".csv", sep="")
        } else if (nchar(i) < 3) {
            filename <- paste(directory, "/0", i, ".csv", sep="")
        } else {
            filename <- paste(directory, "/", i, ".csv", sep="")
        }
        data <- read.csv(filename)
        d[row,2] <- sum(complete.cases(data))
        row <- row + 1
    }
    
    d
}