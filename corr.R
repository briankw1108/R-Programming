library(dplyr)
corr = function(directory, threshold = 0) {
        comp_data = complete(directory, 1:332)
        fil_id = c(filter(comp_data, comp_data$nobs > threshold)$id)
        whole_corr = numeric()
        for (i in fil_id) {
                dataset = read.csv(paste("C://Users/u213493/Desktop/R_Coursera/rprog_data_specdata/", directory, "/", formatC(i, flag = "0", width = 3), ".csv", sep = ""), header = T)
                good_data = dataset[complete.cases(dataset),]
                correlation = cor(good_data$sulfate, good_data$nitrate)
                whole_corr = c(whole_corr, correlation)
        }
        whole_corr
}