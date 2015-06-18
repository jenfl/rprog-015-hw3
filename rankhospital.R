## rankhospital
##    state -- 2 digit abbreviation
##    outcome -- one of "heart attack", "heart failure", "pneumonia"
##    num -- rank (numerical, "best", "worst")
## This function returns the hospital for the given state and outcome
## that occupies the given ranking.  Ties are broken based on alphabetical order.

rankhospital <- function (state, outcome, num="best") {
    
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
 
    ## Subset our data for the given requirements
    st <- subset(measuresCSV, 
                 measuresCSV$State==state & measuresCSV[[mortalityCol]] != "Not Available", 
                 select = c("Hospital.Name", mortalityCol), drop=FALSE)
    
    ## Turn mortality value to numeric
    st[[mortalityCol]] <- as.numeric(st[[mortalityCol]])
}