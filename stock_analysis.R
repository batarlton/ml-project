#install.packages("quantmod",dependencies = TRUE)
library(quantmod)

#Get prices from a certain date to a certain date
getSymbols("AAPL", from = '2016-07-01', to = '2016-07-31')

#Create a chart
chartSeries(AAPL)