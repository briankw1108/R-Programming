library(dplyr)
setwd("C://Users/u213493/Desktop/R_Coursera")
rankhospital = function(state, outcome, num) {
        #read outcome data.
        data = read.csv("outcome-of-care-measures.csv", header = T, colClasses = "character", na.strings = "Not Available")
        
        #check that state and outcome are valid.
        if (!any(state == state.abb)) {
                stop("invalid state")
        }
        if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        #subset the data by the argument of state.
        st = filter(data, State == state)
        
        #set columns of death rate as numeric format instead of character set at beginning.
        st[[11]] = as.numeric(st[[11]])
        st[[17]] = as.numeric(st[[17]])
        st[[23]] = as.numeric(st[[23]])
        
        #return hospital name in that state with the given rank
        sort_st = data.frame()
        if (outcome == "heart attack") {
                st = st[!is.na(st[[11]]), ] #remove missing value in death rate of heart attack column.
                if (num == "best") {
                        sort_st = arrange(st, st[[11]])[1, 2]
                }
                else if (num == "worst") {
                        sort_st = arrange(st, desc(st[[11]]))[1, 2]
                }
                else if (as.integer(num) > nrow(st)) {
                        return(NA)
                }
                else {
                        sort_st = arrange(st, st[[11]])[as.integer(num), 2]
                }
                
        }
        else if (outcome == "heart failure") {
                st = st[!is.na(st[[17]]), ] #remove missing value in death rate of heart failure column.
                if (num == "best") {
                        sort_st = arrange(st, st[[17]])[1, 2]
                }
                else if (num == "worst") {
                        sort_st = arrange(st, desc(st[[17]]))[1, 2]
                }
                else if (as.integer(num) > nrow(st)) {
                        return(NA)      
                }
                else {
                        sort_st = arrange(st, st[[17]])[as.integer(num), 2]
                }
        }
        else {
                st = st[!is.na(st[[23]]), ] #remove missing value in death rate of pneumonia column.
                if (num == "best"){
                        sort_st = arrange(st, st[[23]])[1,2]
                }
                else if (num == "worst") {
                        sort_st = arrange(st, desc(st[[23]]))[1, 2]
                }
                else if (as.integer(num) > nrow(st)) {
                        return(NA)
                }
                else {
                        sort_st = arrange(st, st[[23]])[as.integer(num), 2]
                }
               
        }
        return(sort_st) #return the name of the hospital according to the arguments.
}