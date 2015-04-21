rankhospital <- function(state, outcome, num = "best") {
    result <- data.frame()
    # Reading the outcome file
    outcomeDF <- read.csv('outcome-of-care-measures.csv')
    # Getting state list
    statelist <- unique(outcomeDF[,7])
    
    # Validations
    if (!(state %in% statelist)){
        stop("invalid state")
    }
    if (is.numeric(num) == FALSE) {
        if (!(num %in% c("best", "worst"))) stop("invalid outcome")
    }
    # Getting the column to be read
    coltoread <- if (outcome == "heart attack")  11
            else if (outcome == "heart failure") 17
            else if (outcome == "pneumonia")     23
    
    # Getting the state and corresponding outcome value, data filtered by state and not NA outcome
    result <- subset(outcomeDF[, c(2,coltoread)], outcomeDF[coltoread] != "Not Available" & outcomeDF[7] == state)
    
    # Converting data types for individual columns from the default "factor" datatype
    result[,1] <- as.character(result[,1])
    result[,2] <- as.numeric(paste(result[,2])) # Paste is required to avoid decimal truncation
    
    # Getting the sorted result based on outcome and then on hospital
    final <- result[order (result[2], result[1]), ]
    
    rowcnt <- nrow(final)

    rank <-   if (num == "best")  1
         else if (num == "worst") rowcnt
         else                     num

    if (is.numeric(num) & num > rowcnt) return (NA)

    # Unlink the dataframes
    unlink(outcomeDF)
    
    # Print the hospital name (first row, first column)
    final[rank,1]
}
