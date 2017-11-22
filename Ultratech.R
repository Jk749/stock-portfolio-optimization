library(TTR)
ultratech_web = getYahooData("ULTRACEMC.NS",start = "20110101",end = "20150831") #this also adjusts for split and dividends
class(ultratech_web) #xts class object

ultratech = as.data.frame(ultratech_web)
summary(ultratech)

library(lubridate)
ultratech$day = ymd(rownames(ultratech))
ultratech$mth = month(ultratech$day)
ultratech$yr = year(ultratech$day)
#ultratech = data.frame(ultratech,day,yr,mth)

library(dplyr)

ultratech_Data = ultratech
ultratech = select(ultratech_Data, Price = Close, yr, mth)

a1 = group_by(ultratech, yr, mth)
ultratech = summarise(a1, Stock_price = mean(Price))
rm(list = c("a1","ultratech_web"))

data = data.frame(ultratech$Stock_price)
colnames(data) = c("end")
data = mutate(data,returns=(end-lag(end))*100/lag(end))
ultratech$returns = data$returns
# ultratech$Stock_price = NULL
rm(data)
ultratech = ultratech[-1,]
ultratech = as.data.frame(ultratech)

ultratech_returns = mean(ultratech$returns)
ultratech_risk = sd(ultratech$returns)

