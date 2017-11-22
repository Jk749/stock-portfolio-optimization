rm(list = ls())
setwd("C:\\Users\\Jaya Krishna\\Documents")
getwd()

library(quadprog)
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/hdfc.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/apollo.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/Ultratech.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/Maruti.R")

#hdfc, apollo, Ultratech, maruti_suzuki

returns = c(hdfc_returns,apollo_returns,ultratech_returns,maruti_returns)

#covariance matrix computation
returns_df = data.frame(hdfc = hdfc$returns,apollo = apollo$returns,
                        ultratech = ultratech$returns,maruti = maruti$returns)
cov_returns = cov(returns_df)
D = 2*cov_returns

#Amat
Amat <- cbind(1, diag(ncol(returns_df)))
#bvec
bvec = c(1,rep(0,ncol(returns_df)))
#meq - to specify how many equalities are present
meq = 1

########################################################
max_risk = 1
inc_risk = 0.001
n = 1+(max_risk/inc_risk)
ef = matrix(nrow = n,ncol = (ncol(returns_df)+3))
colnames(ef) = c(colnames(returns_df),"Risk","Return","Ratio")
cnt = 1
for( i in seq(from = 0,to = max_risk,by = inc_risk)){
  dvec = returns*i
  res = solve.QP(D,dvec,Amat,bvec,meq)
  ef[cnt,1:ncol(returns_df)] = res$solution
  ef[cnt,"Risk"] = sqrt(sum(res$solution*colSums(cov_returns*res$solution)))
  ef[cnt,"Return"] = as.numeric(res$solution%*%returns)
  ef[cnt,"Ratio"] = as.numeric(ef[cnt,"Return"]/ef[cnt,"Risk"])
  cnt = cnt+1
}

ef = as.data.frame(ef)
ratio = ef[(ef$Ratio == max(ef$Ratio)),ncol(ef)]
ratio

opt_pt = (ef[(ef$Ratio == max(ef$Ratio)),(1:ncol(returns_df))]*100000)
opt_pt

init_stock_price = c(hdfc = hdfc$Stock_price[1],apollo = apollo$Stock_price[1],
                     ultratech = ultratech$Stock_price[1], maruti = maruti$Stock_price[1])

final_stock_price = c(hdfc = hdfc$Stock_price[nrow(returns_df)],
                      apollo = apollo$Stock_price[nrow(returns_df)],
                      ultratech = ultratech$Stock_price[nrow(returns_df)],
                      maruti = maruti$Stock_price[nrow(returns_df)])

n = round(opt_pt/init_stock_price,digits = 0)
n

exp_returns = n*final_stock_price
sum(exp_returns)

gain = sum(exp_returns)-100000
gain
