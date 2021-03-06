# Analysis of Economic Time Series (R)

# economic time series gathered with this program are continually
# updated... so predictive models, forecasts, and data visualizations
# produced by this program may differ from those shown in the book

library("quantmod",lib.loc ="~/R/win-library/3.4") # use for gathering and charting economic data
library("lubridate",lib.loc="~/R/win-library/3.4") # date functions
library("latticeExtra",lib.loc="~/R/win-library/3.4") # package used for horizon plot
library("forecast",lib.loc="~/R/win-library/3.4") # functions for time series forecasting 
library("lmtest",lib.loc="~/R/win-library/3.4") # for Granger test of causality

par(mfrow = c(2,2)) # four plots on one window/page

# Economic Data from Federal Reserve Bank of St. Louis (FRED system)
# National Civilian Unemployment Rate (monthly, percentage)
getSymbols("UNRATENSA", src="FRED", return.class = "xts")
ER <- 100 - UNRATENSA # convert to employment rate
dimnames(ER)[2] <- "ER"
chartSeries(ER,theme="white")
ER.data.frame <- as.data.frame(ER)
ER.data.frame$date <- ymd(rownames(ER.data.frame))
ER.time.series <- ts(ER.data.frame$ER, 
                     start = c(year(min(ER.data.frame$date)),month(min(ER.data.frame$date))),
                     end = c(year(max(ER.data.frame$date)),month(max(ER.data.frame$date))),
                     frequency=12)

# Manufacturers' New Orders: Durable Goods (millions of dollars) 
getSymbols("DGORDER", src="FRED", return.class = "xts")
DGO <- DGORDER/1000 # convert to billions of dollars
dimnames(DGO)[2] <- "DGO" # use simple name for index
chartSeries(DGO, theme="white") 
DGO.data.frame <- as.data.frame(DGO)
DGO.data.frame$DGO <- DGO.data.frame$DGO
DGO.data.frame$date <- ymd(rownames(DGO.data.frame))
DGO.time.series <- ts(DGO.data.frame$DGO, 
                      start = c(year(min(DGO.data.frame$date)),month(min(DGO.data.frame$date))),
                      end = c(year(max(DGO.data.frame$date)),month(max(DGO.data.frame$date))),
                      frequency=12)

# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
getSymbols("UMCSENT", src="FRED", return.class = "xts")
ICS <- UMCSENT # use simple name for xts object
dimnames(ICS)[2] <- "ICS" # use simple name for index
chartSeries(ICS, theme="white")
ICS.data.frame <- as.data.frame(ICS)
ICS.data.frame$ICS <- ICS.data.frame$ICS
ICS.data.frame$date <- ymd(rownames(ICS.data.frame))
ICS.time.series <- ts(ICS.data.frame$ICS, 
                      start = c(year(min(ICS.data.frame$date)), month(min(ICS.data.frame$date))),
                      end = c(year(max(ICS.data.frame$date)),month(max(ICS.data.frame$date))),
                      frequency=12)

# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
NHS <- HSN1FNSA
dimnames(NHS)[2] <- "NHS" # use simple name for index
chartSeries(NHS, theme="white")
NHS.data.frame <- as.data.frame(NHS)
NHS.data.frame$NHS <- NHS.data.frame$NHS
NHS.data.frame$date <- ymd(rownames(NHS.data.frame))
NHS.time.series <- ts(NHS.data.frame$NHS, 
                      start = c(year(min(NHS.data.frame$date)),month(min(NHS.data.frame$date))),
                      end = c(year(max(NHS.data.frame$date)),month(max(NHS.data.frame$date))),
                      frequency=12)

# define multiple time series object
economic.mts <- cbind(ER.time.series, DGO.time.series, ICS.time.series,
                      NHS.time.series) 
dimnames(economic.mts)[[2]] <- c("ER","DGO","ICS","NHS") # keep simple names 
modeling.mts <- na.omit(economic.mts) # keep overlapping time intervals only

# plot multiple time series 
pdf(file="fig_economic_analysis_mts_R.pdf",width = 8.5,height = 11)    
plot(modeling.mts,main="")
dev.off()
