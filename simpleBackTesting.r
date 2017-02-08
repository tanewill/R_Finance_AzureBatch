rm(list = ls(all = TRUE))
#http://etfprophet.com/days-since-200-day-highs/

require(quantmod)
ticker <- getSymbols('MSFT',from='2010-01-01')
myDF <- MSFT

daysSinceHigh <- function(x, n){
  apply(embed(x, n), 1, which.max)-1
}

myStrat <- function(x, nHold=100, nHigh=200) {
  position <- ifelse(daysSinceHigh(x, nHigh)<=nHold,1,0)
  c(rep(0,nHigh-1),position)
}

myStock <- Cl(myDF)
myPosition <- myStrat(myStock,100,200)
bmkReturns <- dailyReturn(myStock, type = "arithmetic")
myReturns <- bmkReturns*Lag(myPosition,1)
myReturns[1] <- 0

names(bmkReturns) <- ticker
names(myReturns) <- 'Me'

require(PerformanceAnalytics)
charts.PerformanceSummary(cbind(bmkReturns,myReturns))

Performance <- function(x) {
  
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)
  
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
  return(Perf)
}

results <- cbind(Me=Performance(myReturns),SP500=Performance(bmkReturns))
print(results)

testStrategy <- function(myStock, nHold=100, nHigh=200) {
  myPosition <- myStrat(myStock,nHold,nHigh)
  bmkReturns <- dailyReturn(myStock, type = "arithmetic")
  myReturns <- bmkReturns*Lag(myPosition,1)
  myReturns[1] <- 0
  names(bmkReturns) <- 'Index'
  names(myReturns) <- 'Me'
  
  charts.PerformanceSummary(cbind(bmkReturns,myReturns))
  cbind(Me=Performance(myReturns),Index=Performance(bmkReturns))
  
}

#getSymbols('^FTSE',from='1900-01-01')
#getSymbols('DJIA', src='FRED')
#getSymbols('^N225',from='1900-01-01')

#testStrategy(Cl(FTSE),100,200)
#testStrategy(na.omit(DJIA),100,200)
#round(testStrategy(Cl(N225),100,200),8)