## Function to find hospitals with the rank specified by num in the mortality
## rate specified by outcome for all states.

rankall <- function(outcome, num = "best") {
    
    # load data from the file as a data.frame
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    # format arguments
    outcome <- tolower(outcome)
    num <- tolower(as.character(num))
    
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
    
    # find ranked hospitals from all states
    data[,risk] <- suppressWarnings(as.numeric(data[,risk]))
    data <- data[order(data$State, data[,risk], data[,2], na.last=NA),]
    
    # initialize results data frame
    results <- data.frame("Hospital.Name"=NULL, "State"=NULL)
    
    if(num=="best") {
        for(state in unique(data$State)){
            data.st <- data[data$State==state,]
            data.st <- data.st[order(data.st[,risk],data.st[,2],na.last=NA),]
#             results <- rbind(results,
#                              head(data.st[,c(2,7)],1))
            results <- rbind(results,
                             data.st[1,c(2,7)])
        }
    } else{
    
    if(num=="worst") {
        for(state in unique(data$State)){
            data.st <- data[data$State==state,]
            data.st <- data.st[order(data.st[,risk],data.st[,2],na.last=NA),]
#             results <- rbind(results,
#                              tail(data.st[,c(2,7)],1))
            results <- rbind(results,
                             data.st[nrow(data.st),c(2,7)])
        }
    } else {
        num <- as.integer(num)
        for(state in unique(data$State)){
            data.st <- data[data$State==state,]
            data.st <- data.st[order(data.st[,risk],data.st[,2],na.last=NA),]
            if(num > 0 & num <= nrow(data.st)){
                results <- rbind(results,
                                 data.st[num,c(2,7)])
            } else {
                tmp <- data.frame("Hospital.Name"=NA, "State"=state)
                results <- rbind(results, tmp)
                rm(tmp)
            }
        }
    }
    }
    row.names(results) <- results$State
    names(results) <- c("hospital","state")
    return(results)
}