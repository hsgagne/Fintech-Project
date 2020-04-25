setwd("~/Desktop/Sem2/MF740/Project")
prices <- read.csv("Crypto Aug 01,2015 - April 22 2020.csv")
returns <- cbind(prices[8:1249,1],log(prices[8:1249,2:7]/prices[7:1248,2:7]))  # return data for cryptos
market <- apply(prices[7:1249,2:7],1,mean)   # taking mean for equally weighted portfolio
marketr <- log(market[2:1243]/market[1:1242])       # market returns 
market6 <- cbind(marketr,marketr,marketr,marketr,marketr,marketr)
x <-  returns[,2:7]-market6     # 
CSADt <- apply(abs(x), 1, mean)
x1 <- abs(marketr)
x2 <- (marketr*marketr)
reg <- lm(CSADt ~ x1 +x2  )
summary(reg) 

# Newey-West 

coeftest(reg , vcov=NeweyWest(reg))

# Garch 

fit = garchFit(reg ~ garch(1, 1))

# up 
marketup <- array(0, 1242)   
CSADup <- array(0, 1242) 
x<-1
for (i in 1:1242){
  if (marketr[i] > 0){
    marketup[x] = marketr[i]
    CSADup[x] <- CSADt[i]
      x = x+1}
}
marketup <- marketup[1:x-1]
CSADup <- CSADup[1:x-1]
x1up <- abs(marketup)
x2up <- marketup*marketup
regup <- lm(CSADup ~ x1up +x2up  )
summary(regup)
