library(TTR)
maruti_web = getYahooData("MARUTI.NS",start = "20110101",end = "20150831") #this also adjusts for split and dividends
class(maruti_web) #xts class object

maruti = as.data.frame(maruti_web)
summary(maruti)

library(lubridate)
maruti$day = ymd(rownames(maruti))
maruti$mth = month(maruti$day)
maruti$yr = year(maruti$day)
#maruti = data.frame(maruti,day,yr,mth)

library(dplyr)

maruti_Data = maruti
maruti = select(maruti_Data, Price = Close, yr, mth)

a1 = group_by(maruti, yr, mth)
maruti = summarise(a1, Stock_price = mean(Price))
rm(list = c("a1","maruti_web"))

data = data.frame(maruti$Stock_price)
colnames(data) = c("end")
data = mutate(data,returns=(end-lag(end))*100/lag(end))
maruti$returns = data$returns
# maruti$Stock_price = NULL
rm(data)
maruti = maruti[-1,]
maruti = as.data.frame(maruti)

maruti_returns = mean(maruti$returns)
maruti_risk = sd(maruti$returns)

