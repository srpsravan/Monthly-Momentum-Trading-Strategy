rm(list=ls())
library(xts)
######################This section contains the parameters that we can change
initequity = 100000
past_performance_length = 12
SPXMA_length = 10
stocks_to_hold = 20
longest_indicator = max(past_performance_length,SPXMA_length)
from<-as.Date("1990-01-01")
to<-as.Date("2020-04-01")
stockData<-read.csv("C:/Users/jatni/Desktop/Files/School/FINC 485 - QI/Final Project/stockdataset.csv")
SPXdata<- read.csv("C:/Users/jatni/Desktop/Files/School/FINC 485 - QI/Final Project/^GSPC.csv")
print('read files')
####################################################

SPXdata$Date = as.Date(SPXdata$Date,'%Y-%m-%d')
temp.xts = xts(SPXdata[,c(2:7)],SPXdata$Date)
temp.xts = to.monthly(temp.xts)
names(temp.xts)[1]<-"open" #renames columns
names(temp.xts)[2]<-"high"
names(temp.xts)[3]<-"low"
names(temp.xts)[4]<-"close"
names(temp.xts)[5]<-"volume"
SPXdata = data.frame(temp.xts)
SPXdata$date = row.names(SPXdata)
SPXdata = SPXdata[,c('date','open','high','low','close','volume')]
MA = rollmean(SPXdata$close, SPXMA_length)
SPXdata = SPXdata[-(1:9),]
SPXdata$MA = MA
SPXdata$signal = ifelse(SPXdata$MA<SPXdata$close,1,0)

stockData = stockData[,c(2:9)]
stockData$Date<-as.Date(stockData$Date, '%Y-%m-%d') #converts date column to date object
stockData <- subset(stockData,stockData$Date>=from & stockData$Date<=to) 
stockData$Open <- (stockData$Adjusted/stockData$Close)*stockData$Open 
stockData$High <- (stockData$Adjusted/stockData$Close)*stockData$High
stockData$Low <- (stockData$Adjusted/stockData$Close)*stockData$Low
stockData$Close <- stockData$Adjusted

symbols<- unique(stockData$Ticker) #creates a list for iteration later on

stock = list()
for (sym in symbols) {
  if (sym !='BHF' & sym != 'TIE' & sym != 'WM' & sym != 'CBE' & sym != 'AN' & sym != 'MTB'){ #This symbol has bad data
    print(paste("Loading and transforming",sym)) 
    temp<-subset(stockData,stockData$Ticker==sym)
    temp.xts<-xts(temp[,c(3:8)],temp$Date) #creates xts object 
    temp.xts <- to.monthly(temp.xts) #converts to monthly
    names(temp.xts)[1]<-"open" #renames columns
    names(temp.xts)[2]<-"high"
    names(temp.xts)[3]<-"low"
    names(temp.xts)[4]<-"close"
    names(temp.xts)[5]<-"volume"
    names(temp.xts)[6]<-"Adjusted"
    tempdata = data.frame(temp.xts)
    tempdata[,'symbol'] = sym #adds column specifying the symbol for that data
    tempdata[,'date'] = row.names(tempdata)
    tempdata = tempdata[,c('symbol','date','open','high','low','close','volume')]
    stock[[sym]] = tempdata
  }}
date_col = row.names(stock[['AAPL']])
SPXdata = SPXdata[date_col,]



genIndicators=function(stock){   #function used by Ketzenberg to create the signals from rolling regression
  sym<-stock$symbol[1]
  print(paste('Generating Indicators for symbol: ',sym))
  tradedays<-nrow(stock)
  stock$momentum<-0
  stock$topstock = 0
  stock$SPXsignal = 0
  stock$win = 0
  window<-longest_indicator
  if (window<tradedays){
    for (i in window:(tradedays-1)){
      date = stock$date[i]
      sig = SPXdata[date,'signal']
      stock$SPXsignal[i+1] = sig
      price12 = stock$close[i-past_performance_length+1]
      price1 = stock$close[i-1]
      m = price1/price12
      stock$momentum[i+1] = m
    }
  }
  
  return(stock)
}


applyRules=function(stock){  #applies trading rules using the trading signals generated from the genIndicators function
  sym<-stock$symbol[1]
  print(paste('Apply Rules for symbol: ',sym))
  stock$buy<-NA
  stock$sell<-NA
  stock$trade<-ifelse(stock$SPXsignal==1 & stock$topstock ==1 ,1,0)
  
  #stock$short<-ifelse(stock$trade==1&stock$coefficient>0,1,0)
  stock$long<-ifelse(stock$trade==1,1,0)
  
  stock$buy<-ifelse(stock$long==1,stock$open,NA)
  stock$sell<-ifelse(stock$long==1,stock$close,NA)
  stock$valuebought = 0
  stock$valuesold = 0
  return(stock)
}

for (sym in names(stock)){  #runs the genIndicators and applyRules functions
  stock[[sym]] = genIndicators(stock[[sym]])
}

for (date in date_col[(longest_indicator+1):length(date_col)]){
  ranking_df = data.frame(momentum = rep(0,length(names(stock))),filler_col = rep(0,length(names(stock))) ,row.names = (names(stock)))
  for (sym in names(stock)){
    sym_df = stock[[sym]]
    if (date %in% sym_df$date){
      sym_momentum = sym_df[sym_df$date == date,'momentum']
      ranking_df[sym,1] = sym_momentum
    }}
  ranking_df = ranking_df[order(ranking_df$momentum,decreasing = TRUE),]
  ranking_df = ranking_df[1:stocks_to_hold,]
  for (sym in row.names(ranking_df)){
    print(paste(sym,' is a top stock for ',date))
    sym_df = stock[[sym]]
    sym_df[sym_df$date == date,'topstock'] = 1
    stock[[sym]] = sym_df
  }
}

for (sym in names(stock)){  #runs the genIndicators and applyRules functions
  stock[[sym]] = applyRules(stock[[sym]])
}

account = data.frame(row.names = date_col)#creates a stock account 
account$cash = initequity #creates the cash part of the account
account$equity = initequity #creates the equity part of the account

print('calculating performance')
for (i in 1:length(date_col)){  #This is where the meat of the backtesting occurs
  if (i>longest_indicator){ 
    
    begin_cash = account[date_col[i-1],'cash']  
    
    if (SPXdata[date_col[i-1],'signal']==1){
      print(paste('Testing',date_col[i]))
      
      cash_after_trade = c()
      
      for (sym in names(stock)){
        
        df = stock[[sym]]
        
        if (date_col[i] %in% row.names(df)){
          
          if (df[date_col[i],'trade']==1){
            
            size = begin_cash/stocks_to_hold
            buy_price = df[date_col[i],'buy']
            shares_to_buy = floor(size/buy_price)
            value_bought = buy_price*shares_to_buy
            excess_cash = size-value_bought
            sell_price = df[date_col[i],'sell']
            value_sold = sell_price*shares_to_buy
            
            if ((value_bought+excess_cash)<(value_sold+excess_cash)){
              
              df[date_col[i],'win'] = 1
            }
            cash_after_trade = c(cash_after_trade,value_sold+excess_cash)
          }
        }
        stock[[sym]] = df
      }
    }
    if (SPXdata[date_col[i-1],'signal'] ==0){
      cash_after_trade = c(begin_cash)
    }
    account[date_col[i],'equity'] = account [date_col[i],'cash'] = sum(cash_after_trade)
  }
} 
    
    
calcdrawdown = function(data){  #used to calculate the drawdown of the account, takes the account$equity column as a parameter
  data = data.frame(data)
  data$max = data[1,'data']
  for (i in 1:length(row.names(data))){
    if (i>1){
      max_val = max(data[i,'data'],data[i-1,'max'])
      data[i,'max'] = max_val
    }
  }
  data[,'drawdown'] = 100*(1-(data[,'data']/data[,'max']))
  return(data)
}

calcmaxddlength = function(data){ # used to calculate the length of the longest drawdown, takes the account$equity column as a parameter
  data = calcdrawdown(data)
  streak<-0
  maxstreak<-0
  for (i in c(1:nrow(data))){
    streak<-ifelse(data$data[i]<data$max[i],streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  return(maxstreak)
}

calcwinningtrades = function(){ #used to calculate the percentage of winning trades 
  total_trades = 0
  winning_trades = 0
  for (sym in symbols){
    df = stock[[sym]]
    trades = df$trade
    trades = trades[(longest_indicator+1):length(trades)]
    wintrades = df$win
    wintrades = wintrades[(longest_indicator+1):length(trades)]
    total_trades = total_trades+sum(trades,na.rm=TRUE)
    winning_trades = winning_trades+sum(wintrades,na.rm=TRUE)
  }
  return(winning_trades/total_trades)
}

pctchg = function(df,column){
  df[,'pct change'] = NA
  for (i in 1:length(row.names(df))){
    if (i>1){
      prev_val = df[i-1,column]
      current_val = df[i,column]
      change = (current_val-prev_val)/prev_val
      df[i,'pct change'] = change
    }
  }
  return(df)
}

calcportstats = function(equity){ #used to calculate the portfolio stats 
  cumreturn = c(100*(equity[length(equity)]-equity[1])/equity[1],'%') #calculates cumulative return
  cr = 100*(equity[length(equity)]-equity[1])/equity[1]
  annual_return = c(100*(((cr/100)+1)^(1/((nrow(account)/12)))-1),'%')
  pct = (pctchg(account,'equity'))[,3] #finds the weekly pct change of the value of the account
  return = pct[(longest_indicator+1):length(pct)] #trims the pct vector to only include the returns after the "warm-up" days for the regression
  meanreturn = mean(return,na.rm=TRUE) #finds the mean return
  sharpe = meanreturn/sd(return,na.rm=TRUE)*sqrt(52) #calculates annualized sharpe ratio
  maxdrawdown = max(calcdrawdown(equity)$drawdown,na.rm=TRUE) #calculates max drawdown
  drawdown_length = calcmaxddlength(equity) #calculates length of longest drawdown
  drawdown_length = c(drawdown_length,'months')
  pctwinning = calcwinningtrades() #finds percentage of winning trades
  return(list('Cumulative Return'=cumreturn,
              'Annual Return' = annual_return,
              'Sharpe' = sharpe,
              'MaX Drawdown %' = maxdrawdown,
              'Longest Drawdown'=drawdown_length,
              'Percentage Winning Trades' = pctwinning)) #returns percentage of winning trades
  
}

SPXdata$adjusted_data = initequity*(SPXdata$close/SPXdata$close[SPXMA_length])
plot(log(account$equity),type = 'l',col = 'blue')
lines(log(SPXdata$adjusted_data),col = 'red')
calcportstats(account$equity)

