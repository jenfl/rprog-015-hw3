## best(state, outcome), state as 2-character abbreviation
## Reads outcome-of-care-measures.csv and returns a character
## vector with the name of the hospital with the best (lowest)
## 30-day mortality for the specified outcome ("heart attack", 
## "heart failure" or "pneumonia").  Hospitals with no data are
## excluded, tires return the first hospital in alphabetical order.

best <- function (state, outcome) {
    
    ## Read in our data
    measuresCSV <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
    
    ## Check that the state exists in the data
    if (!state %in% measuresCSV$State) {
        stop ("invalid state")
    }
    
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
    
    st <- subset(measuresCSV, measuresCSV$State==state & measuresCSV[[mortalityCol]] != "Not Available", select = c("Hospital.Name", mortalityCol), drop=FALSE)
    st[[mortalityCol]] <- as.numeric(st[[mortalityCol]])
    minMort <- min(st[[mortalityCol]])
    goodHosps <- st[st[[mortalityCol]] == minMort, ]
    goodHosps <- sort(goodHosps$Hospital.Name)
    return (goodHosps[1])
}