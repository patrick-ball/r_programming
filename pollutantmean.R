pollutantmean <- function(directory, pollutant, id=1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of
    ## the CSV files
    
    ## 'pollutant' is a character vector of lenght 1 indicating the name of the
    ## pollutant for which we will calculate the mean; either "Sulfate" or
    ## "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## Returns the mean of the selected pollutant across all monitors indicated
    ## by the 'id' vector, ignoring NA values
    
    polsum = 0 ## will track summed pollutant values across all files
    count = 0 ## will track number of values added to sum
    
    for (i in id) {
        if (nchar(i) < 2) {
            filename <- paste(directory, "/00", i, ".csv", sep="")
        } else if (nchar(i) < 3) {
            filename <- paste(directory, "/0", i, ".csv", sep="")
        } else {
            filename <- paste(directory, "/", i, ".csv", sep="")
        }
        data <- read.csv(filename)
        polsum <- polsum + sum(data[[pollutant]], na.rm=TRUE)
        count <- count + length(data[,pollutant][!is.na(data[,pollutant])])
    }
    
    polsum / count
}
