#install.packages("quantmod",dependencies = TRUE)
library(quantmod)

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2015-01-01', to = '2015-01-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Create matrix for changes
changeMatrix <- matrix(nrow = 1, ncol = 12)

#Get the change in price
changeMatrix[1] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2015-02-01', to = '2015-02-28')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[2] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2015-03-01', to = '2015-03-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[3] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2015-04-01', to = '2015-04-30')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[4] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2015-05-01', to = '2015-05-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[5] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-06-01', to = '2015-06-30')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[6] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-07-01', to = '2015-07-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[7] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-08-01', to = '2015-08-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[8] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-09-01', to = '2015-09-30')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[9] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-10-01', to = '2015-10-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[10] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-11-01', to = '2015-11-30')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[11] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

getSymbols("AAPL", from = '2015-12-01', to = '2015-12-31')

#Put stock info into matrix
stockMatrix <- as.matrix(AAPL)

#Get the change in price
changeMatrix[12] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]

#Create a chart
#chartSeries(AAPL)