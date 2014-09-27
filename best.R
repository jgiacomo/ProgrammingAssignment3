## Function to find the hospital name with the lowest 30 day mortality rate in a
## particular state.

best <- function(state, outcome) {
    # this function will search the outcome-of-care-measures.csv file and return
    # the name of the hospital with the lowest mortality rate in the outcome
    # specified by the outcome argument.
    #
    # input1: two character code for the state
    # intput2: specifies the outcome to check c('heart attack','heat failure',
    # 'pneumonia').
    #
    # output: name of the hospital as a character vector
    
    # format arguments
    state <- toupper(state)
    outcome <- tolower(outcome)
    
    # load data from the file as a data.frame
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    # check the state argument is valid
    if(!(state %in% unique(data[,7]))) {
        stop("invalid state")
    }
    
    # check validity of outcome argument
#     if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
#         stop("invalid outcome")
#     }
    
    # assign column number according to outcome argument
    if(outcome=="heart attack") {
        risk <- 11
    } else {
        if(outcome=="heart failure"){
            risk <- 17
        } else {
            if(outcome=="pneumonia"){
                risk <- 23
            } else {
                stop("invalid outcome")
            }
        }
    }
    
    state.data <- data[data$State==state,]
    state.data <- suppressWarnings(
        state.data[complete.cases(as.numeric(state.data[,risk])),]
    )
    
    lowest.risk <- min(as.numeric(state.data[,risk]))
    best.rows <- which(as.numeric(state.data[,risk])==lowest.risk)
    best.hospitals <- sort(state.data[best.rows,2])
    return(best.hospitals[1])
    
}