best <- function (state, outcome){
    result <- data.frame()
    # Reading the outcome file
    outcomeDF <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    # Getting state list
    statelist <- unique(outcomeDF[,7])
    
    # Validations
    if (!(state %in% statelist)) stop("invalid state")

#    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
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

    # Print the hospital name (first row, first column)
    final[1,1]
}
