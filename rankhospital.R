rankhospital <- function(state, outcome, num = "best") {
    # initialising a new data frame
    result <- data.frame()

    # Reading the outcome file, treating column's class as character
    outcomeDF <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

    # Getting state list
    statelist <- unique(outcomeDF[,7])

    # Validations
    if (!(state %in% statelist)) stop("invalid state") 
    if (is.numeric(num) == FALSE) {
        if (!(num %in% c("best", "worst"))) stop("invalid outcome")
    }

    # Getting the column to be read
    coltoread <- if (outcome == "heart attack")  11
            else if (outcome == "heart failure") 17
            else if (outcome == "pneumonia")     23
            else                                 stop("invalid outcome")
    
    # Getting the state and corresponding outcome value, data filtered by state and not NA outcome
    result <- subset(outcomeDF[, c(2,coltoread)], outcomeDF[coltoread] != "Not Available" & outcomeDF[7] == state)
    
    # Converting outcome values to numeric for sorting purpose
    result[,2] <- as.numeric(result[,2])
    
    # Getting the sorted result based on outcome and then on hospital
    final <- result[order (result[2], result[1]), ]
    
    # Getting row count in sorted dataframe
    rowcnt <- nrow(final)
    
    # Defining the rank to be fetched based on the parameter passed 
    rank <- if (num == "best")  1
       else if (num == "worst") rowcnt
       else                     num
    
    # If the rank passed is more than the number of records in the frame, return NA
    if (is.numeric(num) & num > rowcnt) return (NA)
    
    # Print the hospital name (rank, first column)
    final[rank,1]
}
