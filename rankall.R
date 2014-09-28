## Function to find hospitals with the rank specified by num in the mortality
## rate specified by outcome for all states.

rankall <- function(outcome, num = 'best') {
    
    # load data from the file as a data.frame
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    # find ranked hospitals from all states
    source("rankhospital.R")
    results <- data.frame("Name"=NULL, "State"=NULL)  # initiate data frame
    for(state in unique(data$State)){
        hospital <- rankhospital(state, outcome, num)
        tmp <- data.frame("Name"=hospital, "State"=state)
        results <- rbind(results, tmp)
    }
    
    return(results)
}