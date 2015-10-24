complete = function(directory, id = 1:332) {
        newData = data.frame()
        for (i in id) {
          data = read.csv(paste("C://Users/u213493/Desktop/R_Coursera/rprog_data_specdata/", directory, "/", formatC(i, flag = "0", width = 3), ".csv", sep = ""), header = T)
          compData = data[complete.cases(data),]
          nobs = nrow(compData)
          newDF = data.frame(i, nobs)
          newData = rbind(newData, newDF)
        }
        colnames(newData) = c("id", "nobs")
        return(newData)
}