rankall <- function(outcome, num = "best") {
    # Reading the outcome file
    inputDF <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

    # Validations
    if (is.numeric(num) == FALSE) {
        if (!(num %in% c("best", "worst"))) stop("invalid outcome")
    }
    
    # Getting the column to be read
    coltoread <- if (outcome == "heart attack")  11
            else if (outcome == "heart failure") 17
            else if (outcome == "pneumonia")     23
            else                                 stop("invalid outcome")
    
    # Getting the hospital name, state and outcome value, data filtered by "not NA" outcome
    inputsubDF <- subset(inputDF[, c(2,7,coltoread)], inputDF[coltoread] != "Not Available")
    
    # Assign column names (for clarity)
    names(inputsubDF)[1] <- "hospital"
    names(inputsubDF)[2] <- "state"
    names(inputsubDF)[3] <- "outcome"
    
    # Convert outcome column values to numeric
    inputsubDF[,3] <- as.numeric(inputsubDF[,3])

    # Ordering the data frame by state, outcome and hospital name
    inputsuborderDF <- inputsubDF[order (inputsubDF$state, inputsubDF$outcome,inputsubDF$hospital), ]
    
    # Splitting the DF by state
    inputsubordersplitDF <- split(inputsuborderDF, inputsuborderDF$state)
    
    # Creating vectors to hold state and hospital names
    state.vector    <- character(length(unique(inputDF$State)))
    hospital.vector <- character(length(unique(inputDF$State)))
    
    # Counter
    i <- 1
    
    # For each state, find out the hospital entry and both these values to the vectors
    for (state in names(inputsubordersplitDF)) {
        # Get number of rows in each splitted data set
        rowcnt <- nrow(inputsubordersplitDF[[state]])

        # Derive the exact record count to be fetched
        rank <-   if (num == "best")  1
             else if (num == "worst") rowcnt
             else                     num
        # Add the values to the vecorts
        hospital.vector[i] <- (inputsubordersplitDF[[state]])[rank,1]
        state.vector[i]    <- state
        # increment the counter
        i <- i + 1
    }
    # Create the result DF by adding the two vectors as column
    result <- data.frame(hospital = hospital.vector, state = state.vector)
    
    # Print it
    result
}
