pollutantmean = function(directory, pollutant, id = 1:332) {
        alldata = as.numeric()
        for (i in id) {
                data = read.csv(paste("C://Users/u213493/Desktop/R_Coursera/rprog_data_specdata/", directory, "/", formatC(i, flag = "0", width = 3), ".csv", sep = ""), header = T)
                alldata = c(alldata, data[[pollutant]])
                }
        datamean = mean(alldata, na.rm = T)
        print(datamean)
        }