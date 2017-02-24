#Mikko Salonen
#24.2.2017
#IODS course project data wrangling
#fetching financial data with quantmod and data merging with data from http://data.okfn.org/data/core/s-and-p-500-companies

setwd("./Documents/IODS/git/IODS-final/data/")
tickers <- read.csv("tickers.csv",header=TRUE,sep = ",",dec = ".",row.names = 1,na.strings = "")

#remove unused column
library(dplyr)
summary(tickers)
tickers <- select(tickers, -SEC.Filings)
head(tickers)
str(tickers)
dim(tickers)

#Contentrate on complete cases
tickers_comp <- complete.cases(tickers)
tickers <- tickers[tickers_comp,]
dim(tickers)
str(tickers)

#define diff as percentage difference to price, the higher the more stock has plummeted during the last year
tickers <- mutate(tickers, diff = ((X52.week.high - X52.week.low)/Price*100))
head(tickers)

#Rename columns
colnames(tickers)
tickers <- rename(tickers, Div = Dividend.Yield, PE = Price.Earnings,
                  EPS = Earnings.Share, BV = Book.Value, Low = X52.week.low,
                  High = X52.week.high, Mval = Market.Cap, PS = Price.Sales,
                  PB = Price.Book)
colnames(tickers)


