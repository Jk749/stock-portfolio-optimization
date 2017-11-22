library(TTR)
hdfc_web = getYahooData("HDFCBANK.NS",start = "20110101",end = "20150831") #this also adjusts for split and dividends

class(hdfc_web) #xts class object

hdfc = as.data.frame(hdfc_web)
summary(hdfc)

library(lubridate)
hdfc$day = ymd(rownames(hdfc))
hdfc$mth = month(hdfc$day)
hdfc$yr = year(hdfc$day)
#hdfc = data.frame(hdfc,day,yr,mth)

library(dplyr)

hdfc_Data = hdfc
hdfc = select(hdfc_Data, Price = Close, yr, mth)

a1 = group_by(hdfc, yr, mth)
hdfc = summarise(a1, Stock_price = mean(Price))
rm(list = c("a1","hdfc_web"))

data = data.frame(hdfc$Stock_price)
colnames(data) = c("end")
data = mutate(data,returns=(end-lag(end))*100/lag(end))
hdfc$returns = data$returns
# hdfc$Stock_price = NULL
rm(data)
hdfc = hdfc[-1,]
hdfc = as.data.frame(hdfc)

hdfc_returns = mean(hdfc$returns)
hdfc_risk = sd(hdfc$returns)
