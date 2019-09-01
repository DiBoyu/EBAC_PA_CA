# load packages
pacman::p_load(forecast, tseries, fUnitRoots, tidyverse, fastDummies, lmtest)

# set path to directory
#setwd("/Users/xinxinli/Dropbox/EBAC Master/2019 Semester 2/CA1/data")

# load data
series = read.csv('https://raw.githubusercontent.com/DiBoyu/EBAC_PA_CA/master/AmtrakBig_CA_Question-3.csv?token=ADTTDROVJJYXLILYFSOKNHS5NHGAK')

# data characteristics
head(series, 4)
dim(series)
str(series)

# convert data type to time series
is.ts(series$Ridership)
Rider = ts(series$Ridership, frequency = 12, start = c(2005,1)) #convert data type
is.ts(Rider)

# check time series
start(Rider)
end(Rider)
frequency(Rider)
cycle(Rider)

# plot time series
autoplot(Rider)



