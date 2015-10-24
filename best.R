library(dplyr)
best = function(state, outcome) {
        #read data from directory folder
        data = read.csv("outcome-of-care-measures.csv", header = T, colClasses = "character", na.strings = "Not Available")
        
        #check validity of the arguments
        if (!any(state == state.abb)) {
                stop("invalid state")
        }
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        #subset the data by the argument of state
        st = filter(data, State == state)
        
        #set three death rate column as numeric format
        st[[11]] = as.numeric(st[[11]])
        st[[17]] = as.numeric(st[[17]])
        st[[23]] = as.numeric(st[[23]])
        
        #extract the name of the hospital with the lowest death rate according to the outcomes
        if (outcome == "heart attack") {
                min_death = arrange(filter(st, st[[11]] == min(st[[11]], na.rm = T)), State)
                return(min_death[1, 2])
        } else if (outcome == "heart failure") {
                min_death = arrange(filter(st, st[[17]] == min(st[[17]], na.rm = T)), State)
                return(min_death[1, 2])
        } else {
                min_death = arrange(filter(st, st[[23]] == min(st[[23]], na.rm = T)), State)
                return(min_death[1, 2])
        }
             
}