library(TTR)
apollo_web = getYahooData("APOLLOHOS.NS",start = "20110101",end = "20150831") #this also adjusts for split and dividends
class(apollo_web) #xts class object

apollo = as.data.frame(apollo_web)
summary(apollo)

library(lubridate)
apollo$day = ymd(rownames(apollo))
apollo$mth = month(apollo$day)
apollo$yr = year(apollo$day)
#apollo = data.frame(apollo,day,yr,mth)

library(dplyr)

apollo_Data = apollo
apollo = select(apollo_Data, Price = Close, yr, mth)

a1 = group_by(apollo, yr, mth)
apollo = summarise(a1, Stock_price = mean(Price))
rm(list = c("a1","apollo_web"))

data = data.frame(apollo$Stock_price)
colnames(data) = c("end")
data = mutate(data,returns=(end-lag(end))*100/lag(end))
apollo$returns = data$returns
# apollo$Stock_price = NULL
rm(data)
apollo = apollo[-1,]
apollo = as.data.frame(apollo)

apollo_returns = mean(apollo$returns)
apollo_risk = sd(apollo$returns)

