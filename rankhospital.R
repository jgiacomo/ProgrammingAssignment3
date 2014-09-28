## Function to find the hospital name with the rank specified in the outcome
## specified.

rankhospital <- function(state, outcome, num) {
    # this function will search the outcome-of-care-measures.csv file and return
    # the name of the hospital ranked as specified in the num argument in the
    # outcome specified by the outcome argument.
    #
    # input1: two character code for the state
    # intput2: specifies the outcome to check c('heart attack','heat failure',
    # 'pneumonia').
    # input3: specifies the numeric rank to find.
    #
    # output: name of the hospital as a character vector
    
    # format arguments
    state <- toupper(state)
    outcome <- tolower(outcome)
    num <- tolower(as.character(num))
    
    # load data from the file as a data.frame
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    # check the state argument is valid
    if(!(state %in% unique(data[,7]))) {
        stop("invalid state")
    }
        
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
    
    state.data <- data[data$State==state,]  # pull only data for state
    # change mortality rate data to numeric data
    state.data[,risk] <- suppressWarnings(as.numeric(state.data[,risk]))
    # sort by mortality rate and hospital name
    state.data <- suppressWarnings(
        state.data[order(state.data[,risk],state.data[,2],na.last=NA),]
    )
    
    if(num=='best'){
        return(state.data[1,2])
    }
    if(num=='worst'){
        return(state.data[nrow(state.data),2])
    }
    num <- as.integer(num)
    if(num > 0 & num <= nrow(state.data)){
        return(state.data[num,2])
    }
    if(num < 1 | num > nrow(state.data)){
        return(NA)
    }
}