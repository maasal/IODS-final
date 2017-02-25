#Mikko Salonen
#24.2.2017
#IODS course project data wrangling
#fetching financial data with quantmod and data merging with data from http://data.okfn.org/data/core/s-and-p-500-companies


############
############
# Clean financial data
############
############

setwd("~/Documents/IODS/git/IODS-final/data/")
tickers <- read.csv("tickers.csv",header=TRUE,sep = ",",dec = ".",na.strings = "")

#remove unused column
library(dplyr)
summary(tickers)
tickers <- select(tickers, -SEC.Filings)
head(tickers)
str(tickers)
dim(tickers)

#Contentrate on complete cases, removed 124 companies from SP500
tickers_comp <- complete.cases(tickers)
tickers <- tickers[tickers_comp,]
dim(tickers)
str(tickers)

#define diff as percentage difference to price, the higher the more stock has plummeted during the last year
tickers <- mutate(tickers, diff = ((X52.week.high)/Price*100))
head(tickers)

#Rename columns
colnames(tickers)
tickers <- rename(tickers, Div = Dividend.Yield, PE = Price.Earnings,
                  EPS = Earnings.Share, BV = Book.Value, Low = X52.week.low,
                  High = X52.week.high, Mval = Market.Cap, PS = Price.Sales,
                  PB = Price.Book)

tickers[-c(1:337),c(1,2)]
#remove rows that are not in other data EMC, STJ, TYC rows 115,303, 337
tickers <- tickers[-c(115,303,337),]
dim(tickers)
#####################
#####################
# Fetch new prices
#####################
#####################

#This takes time (approx 9 minutes)!!! We fetch prices between Thursday and Friday and call these new prices
library(quantmod)
newprices <- new.env() 
getSymbols(as.character(tickers[,1]), src = 'yahoo', from = '2017-01-05', to = '2017-01-05', env = newprices, auto.assign = T)
ls.str(newprices)
str(newprices)

# We first have to extract the prices(we use opening price) from the env to list to a data frame, this homemade function does this. 
#Actually pretty proud of this one.
unravel <- function(environ){
  len <- length(as.list(environ))
  frame <- matrix(0,len,2)
  frame[,1] <- names(as.list(environ))
  colnames(frame) <- c("Symbol","NewPrice")
  frame <- as.data.frame(frame)
  temp <- as.data.frame(as.list(environ)[1])[1,1]
  for (i in 1:len-1){
    temp[i+1] <- as.data.frame(as.list(environ)[i+1])[1,1]
  }
  frame[,2] <- as.numeric(temp)
  return(frame)
}
newP <- unravel(newprices)
newP
str(newP)

#Make sure the datas are in line and have the same tickers/symbols
dim(tickers)
dim(newP)
tickers[,1] %in% newP[,1]
colnames(newP)
cbind(tickers[,1],newP[,])
#We now have 377 stocks in this dataset let's combine the two data sets next.
financial <- inner_join(tickers,newP,by = 'Symbol')

#Check with MMM and ABT that the join worked, it did
financial[1:2,]
newP[c(173,371),]
str(financial)

#we still have to count the dependent variable as a difference of the old and the new price
# That is, we want to explain price changes by using the other variables in the data
financial <- mutate(financial, P = (NewPrice - Price) / Price *100)
rownames(financial) <- financial$Symbol
financial <- select(financial, one_of(c("Sector","Div","PE","PS","PB","diff","P")))
financial
str(financial)

#Let's write the data to csv, test it by reading the data. Works fine.
write.csv(x = financial, file = "financial.csv")
dat <- read.csv("financial.csv",row.names = "X")


