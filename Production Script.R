library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
options(warn = -1)
IBport=4002 #7497

# ********************* SET DATE RANGES AND SYSTEM PARAMETERS **********************************

library(quantmod)
library(dplyr)
library(ranger)
library(IBrokers)
longestindicator<-100
currentdate<-Sys.Date()

SPX_ma_length = 200 #length of moving average used for SPX
past_performance_length = 250 #number of days to calculuate past performance on
stocks_to_hold = 20 #number of stocks to hold in portfolio
longest_indicator = max(SPX_ma_length,past_performance_length)

#*************************Create list of symbols currently in SP500, This will be used when downloading data*****************************

datastart<-currentdate-(366) 
tickers<-read.csv("SP Tickers.csv")
additionData<-read.csv("SP Additions.csv")
removalData<-read.csv("SP Removals.csv")
dataMerge<- merge(additionData, removalData, all.x = TRUE, all.y=TRUE) #creates dataframe where each row represents a symbol and includes two columns, addition date and removal date


dataMerge$Date.Added = as.Date(dataMerge[,'Date.Added'],format = '%m/%d/%Y')
dataMerge$Date.Removed = as.Date(dataMerge[,'Date.Removed'],format = '%m/%d/%Y')
current_SPX_symbols = dataMerge[is.na(dataMerge$Date.Removed),]
stocks_to_keep = c()
for (i in 1:nrow(current_SPX_symbols)){
  added = current_SPX_symbols$Date.Added[i]
  if (added<datastart || is.na(added)){ #we allow for added to be NA because the added dates are from wikipedia and wikipedia sometimes lists stocks as being apart of the SP500 and they don't have the added date data
    stocks_to_keep = c(stocks_to_keep,i)
  }
}
current_SPX_symbols = current_SPX_symbols[stocks_to_keep,]
row.names(current_SPX_symbols) = 1:nrow(current_SPX_symbols)

symList<-as.character(current_SPX_symbols$Ticker)


#*******************************Obtain current portfolio value and open positions before orders are submitted******************
tws = twsConnect(port=4002)

acc<-reqAccountUpdates(tws) 
portfolio<-twsPortfolioValue(acc)

print(paste('Current Availabe Funds',acc[[1]]$AvailableFunds[[1]]))
print(paste('Current Cash',as.numeric(acc[[1]]$CashBalance[[1]])))
print(paste('Current Value of Stock Positions',sum(portfolio$marketValue)))
print(paste('Current Value of all Assets',as.numeric(acc[[1]]$CashBalance[[1]])+sum(portfolio$marketValue)))
print('your curruent portfolio holdings')

View(portfolio)
twsDisconnect(tws)

# ********************************* GET DATA FROM YAHOO for individual stocks ********************************************
getData=function(){
  firsttime<-TRUE 
  for (i in 1:length(symList)){
    symData <- dataMerge[which(dataMerge[,'Ticker']==symList[i]),] 
    fromDate <- datastart 
    toDate <- currentdate
    
    tryCatch({
      options("getSymbols.warning4.0"=FALSE) 
      temp<-getSymbols(symList[i], src="yahoo", from=fromDate, to=toDate, auto.assign = FALSE) 
      if(!is.null(temp)){
        tempDF<- data.frame(date=index(temp),temp) 
        temp<-merge(symList[i], tempDF) 
        names(temp)<-c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted") 
        temp$open<-temp$open*temp$adjusted/temp$close  
        temp$low<-temp$low*temp$adjusted/temp$close   
        temp$high<-temp$high*temp$adjusted/temp$close  
        temp$close<-temp$adjusted 
        temp<-temp[,c(1:7)] 
        if(firsttime){
          sdata <- temp 
        }else{
          sdata <- rbind(sdata, temp)  
        }
        firsttime<-FALSE 
        cat("Ticker",symList[i],"added to stock data frame \n")
      }
    },
    error=function(e) {
      cat("ERROR :",conditionMessage(e), "\n")  
    })
  }
  return(sdata) 
}

stockData = getData()
symbols<- unique(stockData$symbol)

#****************************Download data for SP500 index and AGG bond etf**********************************

S<-getSymbols('^GSPC', src="yahoo", from=as.Date("2018-06-01"), to=Sys.Date(), auto.assign = FALSE, warnings = FALSE)
S= data.frame(S)
names(S) = c('open','high','low','close','volume','adjusted')
S = S[-c(1:(nrow(S)-SPX_ma_length)),]
SPXmean = mean(S$close)  #the following two variables (SPXmean and SPXclose) are the only things we will be using later on from this block of code
SPXclose = S$close[SPX_ma_length]


A = getSymbols('AGG', src="yahoo", from=Sys.Date()-5, to=Sys.Date(), auto.assign = FALSE, warnings = FALSE)
A= data.frame(A)
names(A) = c('open','high','low','close','volume','adjusted')
AGG_price = A$close[nrow(A)]

#****************************reconstructs stockdata into a list of dataframes for each stock, esnures each data frame has a year of trading data, if not, then its not added to the list******************
stock = list()
for (sym in symbols) {
  print(paste("Loading and transforming",sym)) 
  temp<-subset(stockData,stockData$symbol==sym)
  if (temp[nrow(temp),'date'] %in% c(currentdate,currentdate-1,currentdate-2,currentdate-3)){
    print(paste('added ',sym,' to stock list'))
    if (nrow(temp)>=past_performance_length){
      stock[[sym]] = temp 
    }
  }
}

symbols = names(stock)


#**************************Calculuates the past year return for each stock and then takes the top X (X is stocks_to_hold) stocks and drops everything else from the dataframe************

ranking_df = data.frame(sym = symbols, momentum = rep(0,length(symbols)), price = rep(0,length(symbols)),position = rep(0,length(symbols)))
for (sym in symbols){
  data = stock[[sym]]
  most_recent_close = data[nrow(data),'close']
  past_close = data[nrow(data)-past_performance_length,'close']
  momentum = most_recent_close/past_close
  print(paste('Calculating momentum for ',sym, momentum))
  ranking_df[ranking_df$sym == sym,'momentum'] = momentum
  ranking_df[ranking_df$sym == sym,'price'] = most_recent_close
}

ranking_df = ranking_df[order(ranking_df$momentum,decreasing = TRUE),]
ranking_df = ranking_df[1:stocks_to_hold,]
row.names(ranking_df) = 1:nrow(ranking_df)

#********************genTrades function used to calculuate how many shares of each stock in the ranking_df our portfolio should contain*****************

genTrades = function(ranking_df,equity){
  trades = ranking_df
  value_to_buy = equity/stocks_to_hold
  for (sym in trades$sym){
    price = trades[trades$sym == sym,'price']
    position = floor(value_to_buy/price)
    trades[trades$sym == sym,'position'] = position
  }
  return(trades)
}

#******* This is a function created by Dr. Ketzenberg to review trades before submitting them******************************************

reviewTrades=function(trades) { #this function is used to review the trades we have generated. It has two arguments, trades and tradetype. trades 
  done<-FALSE
  if (nrow(trades)>0){
    trades<-trades[,c("sym","momentum","price","position")]
    trades$momentum<-round(trades$momentum,digits=4)
    rownames(trades)<-seq(1,nrow(trades),1)
  } else done<-TRUE
  while (!done) {
    View(trades)
    cat("\014")
    choice<-readline(prompt="Choose D)elete trade, M)odify Price, C)hange position, E)xecute, Q)uit without trading:  ")
    choice<-toupper(choice)
    done<-ifelse(choice=="E",TRUE,FALSE)
    if (choice=="Q"){
      trades<-trades[-c(1:nrow(trades)),]
      done<- TRUE
    }
    if (choice=="M"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to modify: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newprice<-as.numeric(readline(prompt=paste("Enter the new limit price for",trades$sym[rownum],": ")))
        valid<-is.numeric(newprice)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$price[rownum]<-newprice
        } else { 
          print("invalid price")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    if (choice=="C"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to change: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newposition<-as.numeric(readline(prompt=paste("Enter the new position size for",trades$sym[rownum],": ")))
        valid<-is.numeric(newposition)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$position[rownum]<-newposition
        } else { 
          print("invalid position")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    if (choice=="D"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to delete: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        confirm<-as.character(readline(prompt=paste("Enter Y)es to confirm, N)o to abort removing the trade for ",trades$sym[rownum],": ")))
        valid<-is.character(confirm)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          if (toupper(confirm)=="Y") {
            trades<-trades[-rownum,]
            if(nrow(trades)==0){
              print("Last candidate trade deleted")
              Sys.sleep(2)
            }
          }
        } else { 
          print("invalid entry")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
  }
  return(trades)
}


#***************In the following section we connect to Interactive brokers and execute trades*************************

tws = twsConnect(port=4002)
acc<-reqAccountUpdates(tws)
portfolio<-twsPortfolioValue(acc)
current_account_val = as.numeric(acc[[1]]$CashBalance[[1]])+sum(portfolio$marketValue)

trades = genTrades(ranking_df, current_account_val)

#**********************First section, this executes if SPXclose>SPXmean. This part scans the portfolio and for any stock in the portfolio that is not in the trades dataframe
#********************* that stock is sold. Then for any stock in the portfolio that is also in the trades dataframe, the current shares for that position is most likely different
#********************* than the desired number of shares in the trades dataframe. The difference between this current and desired amount is calculated and placed into the position
#********************* column in the trades dataframe. Then all trades in the trades dataframe are executed. positive positions represent shares to go long. negative positions represent
#********************* shares to go sell. There are NO short positions. This is a long only strategy

if (SPXclose>SPXmean){
  if (!is.null(portfolio)){
    sell_trades = data.frame(sym = rep(NA,length(portfolio$local)), position = rep(NA, length(portfolio$local)), 
                             row.names = 1:nrow(portfolio))
    for (i in 1:nrow(portfolio)){
      if (!(portfolio$local[i] %in% trades$sym)){
        sell_trades[i,'sym'] = as.character(portfolio$local[i])
        sell_trades[i,'position'] = portfolio$position[i]
      }
    }
    sell_trades = na.omit(sell_trades)
    if (!is.null(sell_trades)){
      row.names(sell_trades) = 1:nrow(sell_trades)
      View(sell_trades)
      cat('\014')
      print('The following positions will be sold')
      choice = readline(prompt = 'Do you wish to sell? Y/N? If you choose No, the program will stop and no trades will be executed: ')
      
      if (choice == 'Y'){
        for (i in 1:nrow(sell_trades)){
          equity<-twsEquity(as.character(sell_trades$sym[i]),'SMART',primary="ISLAND")
          OrdId<-reqIds(tws)
          rowIndx = which(as.character(portfolio$local) == sell_trades$sym[i])
          action <- ifelse(portfolio$position[rowIndx]>0,"SELL","BUY")
          order<-twsOrder(OrdId,action=action,totalQuantity=abs(portfolio$position[rowIndx]),orderType="MKT")
          placeOrder(tws,equity,order)
        }
      }
      
      if (choice == 'N'){
        stop('Program stopped because you chose not to sell trades')
      }
      print('Waiting 4 minutes for orders to fill')
      Sys.sleep(40)
    }
  }
  acc<-reqAccountUpdates(tws)
  portfolio<-twsPortfolioValue(acc)
  for (i in 1:nrow(trades)){
    if (trades$sym[i] %in% portfolio$local){
      sym = trades$sym[i]
      current_position = portfolio[as.character(portfolio$local) == sym, 'position']
      desired_position = trades[i, 'position']
      difference = desired_position - current_position
      trades[i,'position'] = difference
    }
  }
  trades = reviewTrades(trades)
  row.names(trades) = 1:nrow(trades)
  
  for (i in 1:nrow(trades)){
    equity<-twsEquity(as.character(trades$sym[i]),'SMART',primary="ISLAND")
    OrdId<-reqIds(tws)
    action <- ifelse(trades$position[i]>0,"BUY","SELL")
    order<-twsOrder(OrdId,action=action,totalQuantity=abs(trades$position[i]),orderType="MKT")
    placeOrder(tws,equity,order)
  }
}


#****** This section only runs if the SPXclose<SPX mean. In this section if AGG is not in the portfolio, then we sell all positions and buy AGG with 95% of available funds.
#****** We choose 95% because we are determining the number of shares of AGG to buy based off of the most recent closing price for AGG. If this price is significantly 
#****** less than the current price, we may buy far too many shares and end up buying some shares of AGG on margin. If AGG is already in the portfolio, then we do nothing

if(SPXclose<SPXmean){
  if(!is.null(portfolio)){
    if(!('AGG' %in% portfolio$local & nrow(portfolio)==1)){
      sell_trades = data.frame(sym = rep(NA,length(portfolio$local)), position = rep(NA, length(portfolio$local)),
                               row.names = 1:nrow(portfolio))
      for (i in 1:nrow(portfolio)){
        if(as.character(portfolio$local[i]) != 'AGG'){
          sell_trades[i,'sym'] = as.character(portfolio$local[i])
          sell_trades[i,'position'] = as.character(portfolio$position[i])
        }
      }
      sell_trades = na.omit(sell_trades)
      if(!is.null(sell_trades)){
        row.names(sell_trades) = 1:nrow(sell_trades)
        View(sell_trades)
        cat('\014')
        print('The following positions will be sold')
        choice = readline(prompt = 'Do you wish to sell? Y/N? If you choose No, the program will stop and no trades will be executed: ')
        
        if (choice == 'Y'){
          for (i in 1:nrow(sell_trades)){
            equity<-twsEquity(as.character(sell_trades$sym[i]),'SMART',primary="ISLAND")
            OrdId<-reqIds(tws)
            rowIndx = which(as.character(portfolio$local) == sell_trades$sym[i])
            action <- ifelse(portfolio$position[rowIndx]>0,"SELL","BUY")
            order<-twsOrder(OrdId,action=action,totalQuantity=abs(portfolio$position[rowIndx]),orderType="MKT")
            placeOrder(tws,equity,order)
          }
        }
        if (choice == 'N'){
          stop('Program stopped because you chose to not proceed with sell trades')
        }
        
        print('Waiting 4 minutes for orders to fill')
        Sys.sleep(30)
      }
    }
  }
  acc<-reqAccountUpdates(tws)
  portfolio<-twsPortfolioValue(acc)
  current_funds = as.numeric(acc[[1]]$AvailableFunds[1])
  
  shares_of_AGG = floor((current_funds*.95)/AGG_price)
  equity<-twsEquity('AGG','SMART',primary="ISLAND")
  OrdId<-reqIds(tws)
  order<-twsOrder(OrdId,action='BUY',totalQuantity=abs(shares_of_AGG),orderType="MKT")
  placeOrder(tws,equity,order)
  print('Placed order to purchase AGG')
}
print('Finished')
twsDisconnect(tws)
