#load packages
library(dplyr)

#set up working directory to where the file is saved.
setwd("C://Users/u213493/Desktop/Data Science")

rankall = function(outcome, num) {
        #read the data saved in the directory folder and subset the data with 
        load.data = read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", header = T)[, c(2, 7, 11, 17, 23)]
        
        #test the validity of the outcome.
        if(!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        #transform data type of 3 death rate columns from character to numeric.
        load.data[, 3] = as.numeric(load.data[, 3])
        load.data[, 4] = as.numeric(load.data[, 4])
        load.data[, 5] = as.numeric(load.data[, 5])
        
        #rename the column names
        colnames(load.data) = c("hospital", "state", "h.attack.rate", "h.failure.rate", "pneumonia.rate")
        
        #create a function to get column numbers from outcomes.
        reason = function(outcome) {
                if(outcome == "heart attack") 3
                else if(outcome == "heart failure") 4
                else 5
                }
        
        #remove NA in heart attack death rate column.
        data = load.data[!is.na(load.data[[reason(outcome)]]), ] 
        
        #make two new empty data frames for later use.
        all.data = data.frame()
        new.data1 = data.frame()
        
        #extract the data frame from conditions being called.
        if(num == "best") {
                for(i in state.abb) {
                        new.data = filter(data, state == i) #subset data frame with states.
                        new.data = arrange(new.data, new.data[[reason(outcome)]])[1, ] #sort the data by death rate and extract the 1st row.
                        all.data = arrange(rbind(all.data, new.data), state) #combine all data from each state and sort it by state.
                }
        return(all.data[, c(1, 2)]) #return the data frame with only 1st and 2nd columns.
        }
        else if(num == "worst") {
                for(i in state.abb) {
                        new.data = filter(data, state == i) #subset data frame with states.
                        new.data = arrange(new.data, desc(new.data[[reason(outcome)]]))[1, ] #sort the data by death rate and extract the 1st row.
                        all.data = arrange(rbind(all.data, new.data), state) #combine all data from each state and sort it by state.
                }        
        return(all.data[, c(1, 2)]) #return the data frame with only 1st and 2nd columns.
        }
        else {
                for(i in state.abb) {
                        new.data = filter(data, state == i) #subset data frame with states.
                                
                                #test if the number of the rank being called is greater than the total hospital, if so place the value as NA.
                                if (as.integer(num) > nrow(new.data)) {
                                        new.data1 = new.data[1, ]
                                        new.data1[[1]] = NA
                                }
                                
                                #extract the data by the number of ranking being called.
                                else {
                                        new.data1 = arrange(new.data, new.data[[reason(outcome)]])[as.integer(num), ] 
                                }
                        all.data = arrange(rbind(all.data, new.data1), state) #combine all data from each state and sort it by state.      
               }
        return(all.data[, c(1, 2)]) #return the data frame with only 1st and 2nd columns.
        }
}