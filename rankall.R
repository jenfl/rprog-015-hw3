## rankall (outcome, num="best")
## This function returns a 2-column data frame containing the hospital
## in each state that has the ranking specified in num.

rankall <- function(outcome, num="best") {
    
    ## Read in our data
    measuresCSV <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
    
    ## Figure which column we'll need for our outcome, error if unexpected
    if (outcome == "heart attack") {
        mortalityCol <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if (outcome == "heart failure") {
        mortalityCol <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else if (outcome == "pneumonia") {
        mortalityCol <- c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    } else {
        stop("invalid outcome")
    }
    
    ## Just pull out the columns we need
    st <- subset(measuresCSV, 
                 measuresCSV[[mortalityCol]] != "Not Available", 
                 select = c("State", "Hospital.Name", mortalityCol), drop=FALSE)
    
    stateList <- sort(unique(st$State))
    
    ## Split it into a list of one data table per state
    st <- split(st, st$State)
    
    results <- data.frame(hospital=character(), state=character(), 
                          stringsAsFactors=FALSE)
    
    for (state in stateList) {
        
        ## Just pull out our state's hospitals
        sH <- st[[state]]
        
        ## Turn mortality value to numeric
        sH[[mortalityCol]] <- as.numeric(sH[[mortalityCol]])
        
        ## Order them by value then alphabetical
        orderedSH <- sH[order(sH[[mortalityCol]], sH$Hospital.Name),]
        
        ## If num comes in as best or worst, convert it to a value
        if (num == "best") {
            num <- 1
        } else if (num == "worst") {
            num <- length(orderedSH$Hospital.Name)
        } else if (num > length(orderedSH$Hospital.Name)) {
            results <- rbind(results, data.frame(state=state,
                                                 hospital=NA))
            next
        }
        
        results <- rbind(results, data.frame(state=state, 
                                             hospital=orderedSH[num,]$Hospital.Name))
    }
    return(results)
}