source("RH_Algo_MOMO_FUN.R")
# create a connection with Robin Hood
RH = RobinHood(username = PASS$username, password = PASS$password)
# get/load historical data - Should be done before the market opens
PRC = pblapply(as.list(tickers), function(symbol) histData(symbol=symbol,MINUTES=15))
PRC = na.omit(do.call(merge,PRC))
colnames(PRC) = gsub(".Close","",names(PRC)) 
# ****************************************************************************************
# 15-minute bars
TF=15
# to save new timestamps
e <- new.env()
### get Time Sequences for Algo
tmz = getTMZ(TF=TF)
# ****************************************************************************************
# THE FOLLOWING LINE ENSURES THAT THE ALGO START TIMES ARE GREATER THAN THE CURRENT TIME
# **** IF THE "SCAN" BLOCK STOP/FAILS - RE-RUN LINES: 20-48 *****
tmz <- tmz[tmz>Sys.time()]
# ****************************************************************************************
#                               ALGO START
# ****************************************************************************************
SLEEEP(1)  
SCAN <- pblapply(as.list(2:length(tmz)), function(xx){
  IN = Sys.time()
  source("RH_Algo_MOMO_FUN.R")
  # get new data
  newData = getQuoteRH(tickers=tickers,timeStamp=tmz[xx-1], TF=TF)
  assign(paste(tmz[xx-1]),newData,envir = e)
  toRbind = do.call(rbind,eapply(e,merge))
  PRC <- rbind(PRC,toRbind[,names(PRC)])
  
  # calculate MoMo: 
  toOpen = momoSTRAT(PRC)
  
  # Open Position...
  sendOrdersMKT(toOpen,maxAllocPerStock=100,maxSlip=0.05)
  
  # Close Position..
  CLOSED <- closePOS(toOpen) # close positions
  
  OUT = Sys.time()
  # ***************************************************************************************
  # optional: Print out the time it took for algo to make a decision + Stock w/Highest MOMO
  OUTPUT(OUT,IN,toOpen)
  # ***************************************************************************************
  # Sleep until the next bar
  SLEEEP(xx)
})
