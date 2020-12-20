require("pbapply"); require("dplyr"); require("highfrequency"); require("quantmod"); require("data.table")
require("RobinHood");require("lubridate");require("timeDate");require("jsonlite");require("httr")
PASS <- new.env()
assign("username","email@gmail.com",envir = PASS)
assign("password","Password",envir = PASS)
# load current stocks
tickers <- c("AMZN","BIDU","GLD","GOOGL","GS","IWM","NFLX","QQQ","SPY","TWTR","FB")
# ***********************************************************************************************
# gets Historical data for the past week 
histData = function(symbol, MINUTES,TF)
{
  data = get_historicals(RH, symbol, interval = '5minute', span = 'week', tz="America/Los_Angeles") 
  data = data[,1:6]
  data[,"begins_at"]<- sapply(data[,"begins_at"],as.character)
  data[,2:6]<- sapply(data[,2:6],as.numeric)
  colnames(data) <- c("timestamp","Open","Close","High","Low","Volume")
  data$timestamp <- as.POSIXct(as.character(data[,"timestamp"]), tz="America/Los_Angeles", format="%Y-%m-%d %H:%M:%S")
  colnames(data)[2:6] <- paste0(as.character(symbol),".",names(data))[2:6]
  intradata = xts(data[,c(2:6)], order.by=data[,"timestamp"])
  dat = intradata
  dat = make.index.unique(dat)
  # convert bars
  dat = to.period(x=dat,period="minutes",k=MINUTES,indexAt = "startof",name = symbol)
  # time offset: 13:00 only returns the close of the day: Op == Hi == Lo == Cl
  dat = dat["T06:00/T13:00"] 
  Cl(dat)
}
### geQuote latest quote from RH
getQuoteRH = function(tickers,timeStamp,TF)
{
  # get latest quotes
  RH = RobinHood(username = PASS$username, password = PASS$password)
  tmp = get_quote(RH, symbol = tickers, limit_output = FALSE)
  logout(RH)
  # will use the mid-point quote 
  tmp$mid <- round((tmp$bid_price + tmp$ask_price)/2,2)
  tmp = tmp[,c("symbol","mid")]
  tmp2 = t(as.data.frame(tmp$mid))
  colnames(tmp2) = tmp$symbol
  timeStamp = timeStamp - minutes(TF)
  if(as.numeric(format(timeStamp,"%S")) != 0)
  {
  timeStamp = as.POSIXct(format(round(timeStamp, units="hour"), format="%Y-%m-%d %H:%M:%S"))
  }
  CLOSE = xts(tmp2,order.by = timeStamp)
  CLOSE
}
### momo strategy - over a 4 bar period
momoSTRAT = function(PRC){
  # Momentum over a 4-Bar period
  MOMO60 <- na.omit(round(ROC(PRC,n=4,"discrete"),4))
  # Bar to Bar returns
  RETS <- na.omit(round(ROC(PRC,n=1,"discrete"),4))
  # index should match momentum index
  RETS <- RETS[index(MOMO60)]
  # Contains the Column index for the MAXIMUM VALUE of each ROW
  SEQ <- as.numeric(apply(MOMO60,1,which.max))
  # APPLY STRATEGY FOR THE HOLDING PERIOD
  ALL <- do.call(merge.xts,lapply(as.list(1:ncol(RETS)), function(x){
    Lag(reclass(ifelse(SEQ==x,1,0),match.to=PRC))*RETS[,x]
  }))
  ALL[is.na(ALL)]<-0
  # RETURNS for the strategy
  EQT <- reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
  colnames(EQT) <- "MoMoRet"
  cbind(SEQ,round(EQT,4))
}
### RobinHood fractional order requirements
# temporary patch for fractional shares
# readMe: https://github.com/JestonBlu/RobinHood/issues/101
# **********************************************************
stk_order = function (RH, symbol, type, time_in_force, trigger, 
                      price, stop_price = NA, quantity, side) 
{
  check_rh(RH)
  if (!type %in% c("market", "limit")) 
    stop("type must be 'market' or 'limit'")
  if (!time_in_force %in% c("gfd", "gtc", "ioc", "opg")) 
    stop(" time_in_fore must be one of 'gfd', 'gtc', 'ioc', 'opg'")
  if (!trigger %in% c("immediate", "stop")) 
    stop("trigger must be 'immediate' or 'stop'")
  if (trigger == "stop" & is.na(stop_price) == TRUE) 
    stop("stop price cant be null if trigger == 'stop'")
  if (!side %in% c("buy", "sell")) 
    stop("side must be 'buy' or 'sell'")
  if (is.na(stop_price) == TRUE) 
    stop_price <- ""
  quantity <- as.character(quantity)
  price <- as.character(price)
  instrument_url <- paste(api_endpoints(endpoint = "quotes"), 
                          symbol, sep = "")
  instrument <- api_quote(RH, instrument_url)
  instrument_id <- instrument$instrument
  orders <- api_orders(RH = RH, action = "order", instrument_id = instrument_id, 
                       symbol = symbol, type = type, time_in_force = time_in_force, 
                       trigger = trigger, price = price, stop_price = stop_price, 
                       quantity = quantity, side = side)
  return(orders)
}
api_endpoints <- function(endpoint, source = "equity") {
  
  api.endpoint <- list(
    # RobinHood endpoints
    url                = "https://api.robinhood.com/",
    accounts           = "accounts/",
    ach_transfers      = "ach/transfers/",
    ach_relationships  = "ach/relationships/",
    ach_schedules      = "ach/deposit_schedules/",
    forex              = "marketdata/forex/quotes/",
    fundamentals       = "fundamentals/?symbols=",
    historicals        = "quotes/historicals/",
    markets            = "markets/",
    marketdata_options = "marketdata/options/",
    options            = "options/",
    option_positions   = "options/positions/",
    option_orders      = "options/orders/",
    option_instruments = "options/instruments/",
    orders             = "orders/",
    portfolios         = "portfolios/",
    positions          = "positions/",
    quotes             = "quotes/?symbols=",
    tags               = "midlands/tags/tag/",
    instruments        = "instruments/",
    token              = "oauth2/token/",
    revoke_token       = "oauth2/revoke_token/",
    user               = "user/",
    watchlist          = "watchlists/",
    # Nummus endpoints
    url_nummus         = "https://nummus.robinhood.com/",
    accounts_crypto    = "accounts/",
    currency_pairs     = "currency_pairs/",
    holdings_crypto    = "holdings/",
    orders_crypto      = "orders/",
    portfolios_crypto  = "portfolios/"
  )
  
  x <- which(names(api.endpoint) == endpoint)
  
  if (source == "equity") {
    endpoint <- paste(api.endpoint$url, as.character(api.endpoint[x]), sep = "")
  }
  
  if (source == "crypto") {
    endpoint <- paste(api.endpoint$url_nummus, as.character(api.endpoint[x]), sep = "")
  }
  
  
  return(endpoint)
}
api_quote <- function(RH, symbols_url) {
  
  # URL and token
  url <- symbols_url
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET call
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # Check if api did not return any results
  if (nrow(dta) == 0) stop("Symbol not found")
  
  dta <- dta %>%
    dplyr::mutate_at(c("ask_price", "bid_price", "last_trade_price",
                       "last_extended_hours_trade_price",
                       "previous_close", "adjusted_previous_close"), as.numeric) %>%
    dplyr::mutate_at("previous_close_date", lubridate::ymd) %>%
    dplyr::mutate_at("updated_at", lubridate::ymd_hms)
  
  
  return(dta)
}
api_orders <- function(RH, action, status_url = NULL, cancel_url = NULL, 
                       instrument_id = NULL, symbol = NULL, type = NULL,
                       time_in_force = NULL, trigger = NULL, price = NULL, 
                       stop_price = NULL, quantity = NULL,
                       side = NULL, page_size = NULL) {
  
  
  if (action == "order") {
    
    url <- api_endpoints("orders")
    token <- paste("Bearer", RH$tokens.access_token)
    
    detail <- data.frame(account = RH$url.account_id,
                         instrument = instrument_id,
                         symbol = symbol,
                         type = type,
                         time_in_force = time_in_force,
                         trigger = trigger,
                         price = price,
                         stop_price = stop_price,
                         quantity = quantity,
                         side = side,
                         client_id = RH$api_client_id)
    
    # If trigger = "stop" then stop_price must be included, otherwise it must be excluded
    if (trigger == "immediate") {
      detail <- detail[, c("account", "instrument", "symbol", "type", "time_in_force",
                           "trigger", "price", "quantity", "side", "client_id")]
    }
    
    dta <- POST(url = url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token),
                body = mod_json(detail, type = "toJSON"))
    
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename URLs
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
    dta$updated_at <-  lubridate::ymd_hms(dta$updated_at)
    dta$last_transaction_at <-  lubridate::ymd_hms(dta$last_transaction_at)
    dta$created_at <-  lubridate::ymd_hms(dta$created_at)
    dta$fees <- as.numeric(dta$fees)
    dta$cumulative_quantity <- as.numeric(dta$cumulative_quantity)
    dta$stop_price <- as.numeric(dta$stop_price)
    dta$reject_reason <- as.numeric(dta$reject_reason)
    dta$price <- as.numeric(dta$price)
    dta$average_price <- as.numeric(dta$average_price)
    dta$quantity <- as.numeric(dta$quantity)
    
    return(dta)
    
  }
  
  
  if (action == "status") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(status_url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename urls
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
  }
  
  
  if (action == "cancel") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- POST(cancel_url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token))
    
    # Format return
    dta <- mod_json(dta, "fromJSON")
    
  }
  
  
  if (action == "history") {
    
    url <- paste(api_endpoints("orders"), "?page_size=", page_size, sep = "")
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.data.frame(dta$results)
    
  }
  
  return(dta)
  
  
}
### send order to Open 
sendOrdersMKT = function(toOpen,maxAllocPerStock,maxSlip){
  require("RobinHood")
  # create a connection with Robin Hood: 
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # *********************************************************************
  priorBAR   = as.numeric(coredata(toOpen[nrow(toOpen)-1,"SEQ"])) # prior stock highest momo
  stock2Open = as.numeric(coredata(toOpen[nrow(toOpen),"SEQ"]))   # this bar's highest momo
  # get highest momo stock
  tic = names(PRC)[stock2Open]
  # check to see if any stocks that we need to open are already in our portfolio
  # Returns a data frame of stock ownership positions
  positions = get_positions(RH)  # get current positions
  positions = as.data.frame(positions)
  if(nrow(positions) > 0 & priorBAR != stock2Open)
  {
    newOrder = tic # assign the ticker name
  }
  if(nrow(positions) > 0 & priorBAR == stock2Open)
  {
    # check to see if ticker appears in portfolio
    if(length(positions$symbol[positions$symbol == tic]) == 0)
    {
      # ticker does not exists in portfolio
      newOrder = tic # assign the ticker name
    }else{
      # ticker exists in portfolio
      newOrder = NULL 
    }
  }
  # if no new positions exists then open new position
  if(nrow(positions) == 0){newOrder = tic}
  #**********************************************************************
  #  SEND ORDER
  #**********************************************************************
  if(is.null(newOrder)){cat("\nNOTHING TO OPEN!\n")}
  if(!is.null(newOrder)){
    # latest quote for stock
    stkPRC = as.numeric(PRC[nrow(PRC),newOrder])
    # order details
    security     = newOrder
    securityShrs = round(maxAllocPerStock/stkPRC,2)
    ACT          = "buy"
    lmtPRC       = round(stkPRC+maxSlip,2)
    # submit order using fractional shares
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)     
  }
  logout(RH)
  # ************************************************************************
}
### send orders to Close
closePOS = function(toOpen)
{
  require("RobinHood")
  # create a connection with Robin Hood: disable Two-Factor Auth on App
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # *********************************************************************
  priorBAR   = as.numeric(coredata(toOpen[nrow(toOpen)-1,"SEQ"])) # prior stock highest momo
  stock2Open = as.numeric(coredata(toOpen[nrow(toOpen),"SEQ"]))   # this bar's highest momo
  # get prior bar stock
  tic = names(PRC)[priorBAR]
  # check to see if any stocks that we need to open are already in our portfolio
  # Returns a data frame of stock ownership positions
  positions = get_positions(RH)  # get current positions
  positions = as.data.frame(positions)
  if(nrow(positions) > 0 & priorBAR != stock2Open)
  {
    # check to see if ticker appears in portfolio
    if(length(positions$symbol[positions$symbol == tic]) != 0)
    {
      newOrder = tic # assign the ticker name
    }else{
      newOrder = NULL # ticker isnt in portfolio
    }
  }
  if(nrow(positions) > 0 & priorBAR == stock2Open)
  {
    # check to see if ticker appears in portfolio
    if(length(positions$symbol[positions$symbol == tic]) == 0)
    {
      # ticker does not exists in portfolio
      newOrder = NULL # does not exist -> don't send order
    }else{
      # ticker exists in portfolio
      # if its in the portfolio don't close as
      # priorBAR == stock2Open
      newOrder = NULL 
    }
  }
  # if no new positions exists then open new position
  if(nrow(positions) == 0){newOrder = NULL}
  #**********************************************************************
  #  SEND ORDER
  #**********************************************************************
  if(is.null(newOrder)){cat("\nNOTHING TO CLOSE!\n")}
  #**********************************************************************
  #  SEND ORDERS
  #**********************************************************************
  if(!is.null(newOrder)){
    # latest quote for stock
    stkPRC = as.numeric(PRC[nrow(PRC),newOrder])
    # order details
    security     = newOrder
    securityShrs = as.numeric(subset(positions,positions$symbol == security)$quantity)
    ACT          = "sell"
    lmtPRC       = round(stkPRC,2)
    # submit order using fractional shares
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)     
  }
  logout(RH)
  #*********************************************************************************************
}
### DETERMINE TRADING DAY: (local time)
DAYTODAY = function()
{
  # time difference between current time zone and NY-time
  tmDIFF = round(as.numeric(difftime(Sys.time(),lubridate::force_tz(with_tz(Sys.time(),"EST")),
                                     units = "hours")),0)
  NOW <- Sys.time()
  # if the time now is past the market close but less than midnight -> trading day will be the next day
  if(NOW > as.POSIXct(paste0(Sys.Date(), " 16:00:00"))+hours(tmDIFF) & NOW < as.POSIXct(paste0(Sys.Date(), " 23:59:59")))
  {
    daytoday <- format(Sys.Date()+1, "%Y%m%d")
  }else{
    # otherwise TODAY is the trading day 
    daytoday <- format(Sys.Date(), "%Y%m%d")
  }
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(xx){
  ttt <- tmz[xx] - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt)
  }  
}
### PRINT OUT to CONSOLE
OUTPUT= function(OUT,IN,toOpen)
{
  cat("\n\n",OUT-IN,"\nBEST MOMO STOCK:\n")
  stock2Open = as.numeric(coredata(toOpen[nrow(toOpen),"SEQ"]))   # this bar's highest momo
  cat(paste0(names(PRC)[stock2Open], " @ $", PRC[nrow(PRC),names(PRC)[stock2Open]]),"\n")
}
## Create sequence of times
getTMZ = function(TF)
{
    # time difference between current time zone and NY-time
    tmDIFF = round(as.numeric(difftime(Sys.time(),lubridate::force_tz(with_tz(Sys.time(),"EST")),
                                 units = "hours")),0)
    # determines the trading day to start
    daytoday <- DAYTODAY()
    # IT WILL MAKE A DECISION AT THE CLOSE OF THE FIRST BAR! 
    START <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 09:30:00"))  # NY TIME
    START <- START + minutes(TF)                                                 # Adjust for Bar Time
    START <- START + hours(tmDIFF)                                               # local Time
    END <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 17:00:00"))    # NY TIME
    END <- END + hours(tmDIFF)                                                   # local Time
    # the following line will determine the start times for the algo
    tmz <- seq(START,END, by=paste0("",TF," min"))
    # MAKE A DECISION RIGHT BEFORE THE CLOSE 
    lastBAR = as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 15:59:55")) # NY TIME - 5 secs before close
    tmz[(length(tmz)-1)] <- lastBAR + hours(tmDIFF)
    # ALGO STOP TIME
    stopALGO = as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:01:00")) # NY TIME - 1 min after close
    tmz[(length(tmz))] <- stopALGO + hours(tmDIFF)
    tmz
}
