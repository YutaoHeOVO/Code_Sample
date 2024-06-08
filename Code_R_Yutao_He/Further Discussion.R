##Clear memory of working space.
remove(list=ls())

## Set up working directory.
getwd()
setwd("../Data")
library(readr)
Japan <- read_delim("Japan.csv", delim = ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(Japan)<-c("Num","Tick","Open","High","Low","Close","Volume")
US<- read_delim("US.csv", delim = ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(US)<-c("Num","Tick","Open","High","Low","Close","Volume")

## Extract Japanese market time.
Japan$Date <- substr(Japan$Tick,1,10)
Japan$Date<-as.Date(Japan$Date)
Japan$DateNum <- as.integer(Japan$Date)
Japan$Minute <- as.integer(substr(Japan$Tick,15,16))+as.integer(substr(Japan$Tick,12,13))*60
Japan$Year <- as.integer(substr(Japan$Tick,1,4))
Japan$Month <- as.integer(substr(Japan$Tick,6,7))
Japan$Day <- as.integer(substr(Japan$Tick,9,10))
Japan$Weekdays <- weekdays(Japan$Date)

## Extract U.S. market time.
US$Date <- substr(US$Tick,1,10)
US$Date<-as.Date(US$Date)
US$DateNum <- as.integer(US$Date)
US$Minute <- as.integer(substr(US$Tick,15,16))+as.integer(substr(US$Tick,12,13))*60
US$Year <- as.integer(substr(US$Tick,1,4))
US$Month <- as.integer(substr(US$Tick,6,7))
US$Day <- as.integer(substr(US$Tick,9,10))
US$Weekdays <- weekdays(US$Date)

## Add holiday dummy.
## U.S.
DateNum <- sort(unique(US$DateNum))
DateNum<- as.data.frame(DateNum)
DateNum[1,2]<-1
for (i in 2:length(DateNum$DateNum)){
  DateNum[i,2]<-DateNum[i,1]-DateNum[i-1,1]
}
DateNum[,3]=0
DateNum[DateNum$V2!=1,3]<-1
names(DateNum)<-c("DateNum","Difference","Holiday")
US<-merge(US,DateNum)
US[US$Weekdays=="Monday",c("Holiday")]<-1
## Japan
DateNum <- sort(unique(Japan$DateNum))
DateNum<- as.data.frame(DateNum)
DateNum[1,2]<-1
for (i in 2:length(DateNum$DateNum)){
  DateNum[i,2]<-DateNum[i,1]-DateNum[i-1,1]
}
DateNum[,3]=0
DateNum[DateNum$V2!=1,3]<-1
names(DateNum)<-c("DateNum","Difference","Holiday")
Japan<-merge(Japan,DateNum)
Japan[Japan$Weekdays=="Sunday",c("Holiday")]<-1


## Daylight saving hour adjustment.
library(readxl)
EDT <- read_excel("EDT.xlsx")
## Adjust month.
US <- merge(US,EDT)
US[US$Month>=US$StM&&US$Month<=US$EnM,c("Minute")]<-US[US$Month>=US$StM&&US$Month<=US$EnM,c("Minute")]+60
## Adjust date.
US[US$Month==US$StM&&US$Day<US$StD,c("Minute")]<-US[US$Month==US$StM&&US$Day<=US$StD,c("Minute")]-60
US[US$Month==US$EnM&&US$Day>US$EnD,c("Minute")]<-US[US$Month==US$EnM&&US$Day>US$EnD,c("Minute")]-60

## Extract U.S. market data at 9:30, 10:00
US930 <- US[US$Minute==570,c("Date","Open","Holiday")]
names(US930) <- c("Date","P1000","Holiday")
US1000 <- US[US$Minute==600,c("Date","Open","Holiday")]
names(US1000) <- c("Date","P1000","Holiday")
USClose <- US[US$Minute==959,c("Date","Close","Holiday")]
## Extract Japan data at 9:00,
JP900 <- Japan[Japan$Minute==1200,c("Date","Open","Holiday")]
names(JP900)<-c("Date","P900","Holiday")
JP915 <- Japan[Japan$Minute==1215,c("Date","Open","Holiday")]
names(JP915)<-c("Date","P915","Holiday")
JP1000 <- Japan[Japan$Minute==1260,c("Date","Open","Holiday")]
names(JP1000)<-c("Date","P915","Holiday")
JPClose <- Japan[Japan$Minute==120,c("Date","Open","Holiday")]
names(JPClose)<-c("Date","Close")
## Adjust date. (Japan)
JP900$Date <- JP900$Date+1
JP915$Date <- JP915$Date+1
JP1000$Date <- JP1000$Date+1

## Merge dataset.
JPData <- merge(JP915,JPClose)
USData <- merge(US1000,USClose)

## Compute SPD, SPN, NKD, NKN
for (i in 2:length(USData$Date)){
  USData[i,5]<-log(USData$Close[i]/USData$P1000[i])
  USData[i,6]<-log(USData$P1000[i]/USData$Close[i-1])
}
names(USData)<-c("Date","USDH","USP1000","USClose","SPD","SPN")
USData <- na.omit(USData)
USData$SPDSq<-USData$SPD*USData$SPD
for (i in 1:(length(JPData$Date)-1)){
  JPData[i,5]<-log(JPData$Close[i]/JPData$P915[i])
  JPData[i,6]<-log(JPData$P915[i+1]/JPData$Close[i])
}
names(JPData)<-c("Date","JPP915","JPDH","JPClose","NKD","NKN")
JPData <-na.omit(JPData)
JPData$NKDSq<-JPData$NKD*JPData$NKD

library(rugarch)
library(fBasics)
##Comtemporaneous Return
###NIKKEI225->S&P500
####Step 1
Spec1A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(JPData$JPDH[1:length(JPData$JPDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(JPData$NKN[1:length(JPData$NKN)-1],JPData$JPDH[1:length(JPData$JPDH)-1]),include.mean = TRUE))
Model1A<- ugarchfit(Spec1A,JPData$NKD[2:length(JPData$NKD)])
show(Model1A)
JPData$JPResid[2:length(JPData$Date)]<-Model1A@fit$residuals
####Step 2
TempData<-merge(JPData,USData)
TempData<-na.omit(TempData)
Spec1B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$USDH[1:length(TempData$USDH)-1],TempData$NKDSq[1:length(TempData$NKDSq)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$SPD[1:length(TempData$SPD)-1],TempData$USDH[1:length(TempData$USDH)-1],TempData$JPResid[1:length(TempData$JPResid)-1]),include.mean = TRUE))
Model1B <- ugarchfit(Spec1B,TempData$SPN[2:length(TempData$SPN)])
show(Model1B)

###S&P500->NIKKEI225
####Step 1
Spec2A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(USData$USDH)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(USData$SPN,USData$USDH),include.mean = TRUE))
Model2A<- ugarchfit(Spec2A,USData$SPD[1:length(USData$SPD)])
show(Model2A)
USData$USResid<-USData$SPN-Model2A@fit[["fitted.values"]]
####Step 2
TempData<-merge(JPData,USData)
TempData<-na.omit(TempData)
Spec2B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$JPDH,TempData$SPDSq)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$NKD,TempData$JPDH,TempData$USResid),include.mean = TRUE))
Model2B <- ugarchfit(Spec2B,TempData$NKN)
show(Model2B)

##Lagged Spillover
###NIKKEI 225->S&P500
Spec3 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$USDH,TempData$NKDSq)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$NKD,TempData$SPN,TempData$USDH)))
Model3<- ugarchfit(Spec3,TempData$SPD)
show(Model3)
###S&P500 -> NIKKEI225
Spec4 <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$JPDH[2:length(TempData$JPDH)],TempData$SPDSq[1:length(TempData$USDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$SPD[1:length(TempData$SPD)-1],TempData$NKN[1:length(TempData$NKN)-1],TempData$JPDH[2:length(TempData$JPDH)])))
Model4<- ugarchfit(Spec4,TempData$NKD[2:length(TempData$NKD)])
show(Model4)

