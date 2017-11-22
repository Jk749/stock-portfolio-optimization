rm(list = ls())
setwd("C:\\Users\\Jaya Krishna\\Documents")
getwd()

source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/hdfc.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/apollo.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/Ultratech.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/Maruti.R")
source("C:/Users/Jaya Krishna/Downloads/Insofe/Projects/Return_risk code/genAlgFun.R")

#hdfc, apollo, Ultratech, maruti_suzuki

returns = c(hdfc_returns,apollo_returns,ultratech_returns,maruti_returns)
returns_df = data.frame(hdfc = hdfc$returns,apollo = apollo$returns,
                        ultratech = ultratech$returns,maruti = maruti$returns)
cov_returns = cov(returns_df)

eval<-function(individual){
  
  returns = c(hdfc_returns,apollo_returns,ultratech_returns,maruti_returns)
  returns_df = data.frame(hdfc = hdfc$returns,apollo = apollo$returns,
                          ultratech = ultratech$returns,maruti = maruti$returns)
  cov_returns = cov(returns_df)
  num=0
  for(i in 1:length(individual)){
    num = num + (individual[i]*returns[i])
  }
  den = sqrt(sum(individual*colSums(cov_returns*individual)))
  fitness = num/den
  return(fitness)
}

#arithmetic crossover

crossOver=function(p1,p2){
  wt=runif(1,0,1)
  #cat(wt)
  bred=wt*p1 +(1-wt)*p2
  return(bred)
}

#mutate

mutate<-function(individual){
  
  a=sample(1:(length(individual)-1),2)
  #cat(a)
  if(a[1]<a[2]){
    k=individual[a[1]]
    individual = c(individual[1:a[1]-1],individual[(a[1]+1):a[2]],k,individual[(a[2]+1):length(individual)])
  }
  else{
    k=individual[a[2]]
    individual = c(individual[1:a[2]-1],individual[(a[2]+1):a[1]],k,individual[(a[1]+1):length(individual)])
  }
  return(individual)
}

p=runif(1000,0,1)
q=runif(1000,0,1)
r=runif(1000,0,1)
s=runif(1000,0,1)

initPop=cbind(p,q,r,s)

res = geneticAlgo(initPop, eval, mutate, 
            crossOver,0.5,0.5,100)

x = c(res$p,res$q,res$r,res$s)
x
y=0
for(i in 1:4){
  y[i]=x[i]/sum(x)
}
y = y*100000
y

init_stock_price = c(hdfc = hdfc$Stock_price[1],apollo = apollo$Stock_price[1],
                     ultratech = ultratech$Stock_price[1], maruti = maruti$Stock_price[1])

final_stock_price = c(hdfc = hdfc$Stock_price[nrow(returns_df)],
                      apollo = apollo$Stock_price[nrow(returns_df)],
                      ultratech = ultratech$Stock_price[nrow(returns_df)],
                      maruti = maruti$Stock_price[nrow(returns_df)])

n = round(y/init_stock_price,digits = 0)
n

exp_returns = n*final_stock_price
sum(exp_returns)

gain = sum(exp_returns)-100000
gain
