##Clear the working space.
remove(list=ls())

## Set up the directory.
getwd()
setwd("../Data")
library(readr)
Japan <- read_delim("Japan.csv", delim = ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(Japan)<-c("Num","Tick","Open","High","Low","Close","Volume")
US<- read_delim("US.csv", delim = ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(US)<-c("Num","Tick","Open","High","Low","Close","Volume")

## Extract time stamp in Japan.
Japan$Date <- substr(Japan$Tick,1,10)
Japan$Date<-as.Date(Japan$Date)
Japan$DateNum <- as.integer(Japan$Date)
Japan$Minute <- as.integer(substr(Japan$Tick,15,16))+as.integer(substr(Japan$Tick,12,13))*60
Japan$Year <- as.integer(substr(Japan$Tick,1,4))
Japan$Month <- as.integer(substr(Japan$Tick,6,7))
Japan$Day <- as.integer(substr(Japan$Tick,9,10))
Japan$Weekdays <- weekdays(Japan$Date)

## Extract time stamp in US market.
US$Date <- substr(US$Tick,1,10)
US$Date<-as.Date(US$Date)
US$DateNum <- as.integer(US$Date)
US$Minute <- as.integer(substr(US$Tick,15,16))+as.integer(substr(US$Tick,12,13))*60
US$Year <- as.integer(substr(US$Tick,1,4))
US$Month <- as.integer(substr(US$Tick,6,7))
US$Day <- as.integer(substr(US$Tick,9,10))
US$Weekdays <- weekdays(US$Date)

## Data processing：
## Add Holiday Dummy Label
## US Market
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

## Japan Market
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


## Load EDT Data for daylight saving hour adjustment.
library(readxl)
EDT <- read_excel("EDT.xlsx")

## Adjust for months of switching.
US <- merge(US,EDT)
US[US$Month>=US$StM&&US$Month<=US$EnM,c("Minute")]<-US[US$Month>=US$StM&&US$Month<=US$EnM,c("Minute")]+60
## Adjust on a daily basis.
US[US$Month==US$StM&&US$Day<US$StD,c("Minute")]<-US[US$Month==US$StM&&US$Day<=US$StD,c("Minute")]-60
US[US$Month==US$EnM&&US$Day>US$EnD,c("Minute")]<-US[US$Month==US$EnM&&US$Day>US$EnD,c("Minute")]-60

## Extract U.S. Data: 9:30
US930 <- US[US$Minute==570,c("Date","Open","Holiday")]
names(US930) <- c("Date","P930","Holiday")
US1000 <- US[US$Minute==600,c("Date","Open","Holiday")]
names(US1000) <- c("Date","P1000","Holiday")
USClose <- US[US$Minute==959,c("Date","Close","Holiday")]

## Extract Japan Data.
JP900 <- Japan[Japan$Minute==900,c("Date","Open","Holiday")]
names(JP900)<-c("Date","P900","Holiday")
JP915 <- Japan[Japan$Minute==1215,c("Date","Open","Holiday")]
names(JP915)<-c("Date","P915","Holiday")
JP1000 <- Japan[Japan$Minute==1260,c("Date","Open","Holiday")]
names(JP1000)<-c("Date","P1000","Holiday")
JPClose <- Japan[Japan$Minute==120,c("Date","Open","Holiday")]
names(JPClose)<-c("Date","Close")

## Adjust the trading days. (Note: Japan is around one day ahead of U.S.)
JP900$Date <- JP900$Date+1
JP915$Date <- JP915$Date+1
JP1000$Date <- JP1000$Date+1

## Merge the dataset.
JPData <- merge(JP915,JPClose)
USData <- merge(US1000,USClose)

## Compute SPD, SPN, NKD, NKN and drop nulls.
for (i in 2:length(USData$Date)){
  USData[i,5]<-log(USData$Close[i]/USData$P1000[i])
  USData[i,6]<-log(USData$P1000[i]/USData$Close[i-1])
}
names(USData)<-c("Date","USDH","USP1000","USClose","SPD","SPN")
USData <- na.omit(USData)
for (i in 1:(length(JPData$Date)-1)){
  JPData[i,5]<-log(JPData$Close[i]/JPData$P915[i])
  JPData[i,6]<-log(JPData$P915[i+1]/JPData$Close[i])
}
JPData <-na.omit(JPData)
names(JPData)<-c("Date","JPP915","JPDH","JPClose","NKD","NKN")

##Aggregate Shock Model Step Up
library(rugarch)
library(fBasics)

###NIKKEI225->S&P500
####Step 1
Spec1A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(JPData$JPDH[1:length(JPData$JPDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(JPData$NKN[1:length(JPData$NKN)-1],JPData$JPDH[1:length(JPData$JPDH)-1]),include.mean = TRUE))
Model1A<- ugarchfit(Spec1A,JPData$NKD[2:length(JPData$NKD)])
show(Model1A)
JPData$JPResid[2:length(JPData$Date)]<-Model1A@fit$residuals
JPData$JPVol[2:length(JPData$Date)]<-Model1A@fit$sigma
re <- residuals(Model1A,standardize=TRUE)
resq <- re^2
skewness(re)
kurtosis(re)
Box.test(re,lag=12,"Ljung-Box")
Box.test(resq,lag=12,"Ljung-Box")

####Step 2
TempData<-merge(JPData,USData)
TempData<-na.omit(TempData)
Spec1B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$USDH[1:length(TempData$USDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$SPD[1:length(TempData$SPD)-1],TempData$USDH[1:length(TempData$USDH)-1],TempData$JPResid[1:length(TempData$JPResid)-1]),include.mean = TRUE))
Model1B <- ugarchfit(Spec1B,TempData$SPN[2:length(TempData$SPN)])
show(Model1B)
re <- residuals(Model1B,standardize=TRUE)
resq <- re^2
skewness(re)
kurtosis(re)
Box.test(re,lag=12,"Ljung-Box")
Box.test(resq,lag=12,"Ljung-Box")

###S&P500->NIKKEI225 shock transmission.
####Step 1
Spec2A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(USData$USDH)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(USData$SPN,USData$USDH),include.mean = TRUE))
Model2A<- ugarchfit(Spec2A,USData$SPD)
show(Model2A)
USData$USResid<-USData$SPN-Model2A@fit[["fitted.values"]]
USData$USVol<-Model2A@fit$sigma
re <- residuals(Model2A,standardize=TRUE)
resq <- re^2
skewness(re)
kurtosis(re)
Box.test(re,lag=12,"Ljung-Box")
Box.test(resq,lag=12,"Ljung-Box")
####Step 2
TempData<-merge(JPData,USData)
TempData<-na.omit(TempData)
Spec2B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$JPDH)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$NKD,TempData$JPDH,TempData$USResid),include.mean = TRUE))
Model2B <- ugarchfit(Spec2B,TempData$NKN)
show(Model2B)
re <- residuals(Model2B,standardize=TRUE)
resq <- re^2
skewness(re)
kurtosis(re)
Box.test(re,lag=12,"Ljung-Box")
Box.test(resq,lag=12,"Ljung-Box")


##Signal Extraction
###NIKKEI225->S&P500
f1A<-function(cf,af,bf,omegag,alphag,betag,gammag,omegah,alphah,betah,gammah,cd,ad,bd,mu,omegak,alphak,betak,gammak){
  ### Set initial value.
  summ<-0
  w<-0.7*TempData$JPResid[1]
  u<-0.3*TempData$JPResid[1]
  g<-10/3*TempData$JPVol[1]
  h<-10/7*TempData$JPVol[1]
  v<-TempData$USResid[1]
  k<-TempData$USVol[1]
  for (i in 2:length(TempData$Date))
  {
    e<-TempData$NKD[i]-cf-af*TempData$NKN[i-1]-bf*TempData$JPDH[i]
    var<-g*h/(g+h)
    g<-abs(omegag+betag*g+alphag*(w*w+var)+gammag*TempData$JPDH[i])
    h<-abs(omegah+betah*h+alphah*(u*u+var)+gammah*TempData$JPDH[i])
    w<-e*g/(g+h)
    u<-e*h/(g+h)
    v<-TempData$SPN[i]-cd-ad*TempData$SPD[i-1]-bd*TempData$USDH[i]-mu*w
    k<-abs(omegak+alphak*v*v+betak*k+gammak*TempData$USDH[i])
    summ<-summ-log(2*3.1415926)+1/2*(log(g+h)+e*e/(g+h)+log(k)+v*v/k)
  }
  return (summ)
}

library(bbmle)
## Conduct MLE estimation.
SignalModelMLE1A<-mle2(f1A,start=list(cf=Model1A@fit$coef[1],af=Model1A@fit$coef[2],bf=Model1A@fit$coef[3],omegag=Model1A@fit$coef[4],alphag=Model1A@fit$coef[5],betag=Model1A@fit$coef[6],gammag=Model1A@fit$coef[7],omegah=Model1A@fit$coef[4],alphah=Model1A@fit$coef[5],betah=Model1A@fit$coef[6],gammah=Model1A@fit$coef[7],cd=Model1B@fit$coef[1],ad=Model1B@fit$coef[2],bd=Model1B@fit$coef[3],mu=Model1B@fit$coef[4],omegak=Model1B@fit$coef[5],alphak=Model1B@fit$coef[6],betak=Model1B@fit$coef[7],gammak=Model1B@fit$coef[8]),"Nelder-Mead")
Result1A<-SignalModelMLE1A@coef
Result1A

Var<-solve(SignalModelMLE1A@details$hessian)
stderror1<-c()
for (i in 1:19){
  stderror1[i]<-sqrt(abs(Var[i,i]))
}
stderror1
Result1A/stderror1

###S&P500->NIKKEI225
f2A<-function(cf,af,bf,omegag,alphag,betag,gammag,omegah,alphah,betah,gammah,cd,ad,bd,mu,omegak,alphak,betak,gammak){
  ## Set initial value.
  summ<-0
  w<-0.7*TempData$USResid[1]
  u<-0.3*TempData$USResid[1]
  g<-10/3*TempData$USVol[1]
  h<-10/7*TempData$USVol[1]
  v<-TempData$JPResid[1]
  k<-TempData$JPVol[1]
  for (i in 2:length(TempData$Date))
  {
    e<-TempData$SPD[i]-cf-af*TempData$SPN[i]-bf*TempData$USDH[i]
    var<-g*h/(g+h)
    g<-abs(omegag+betag*g+alphag*(w*w+var)+gammag*TempData$USDH[i])
    h<-abs(omegah+betah*h+alphah*(u*u+var)+gammah*TempData$USDH[i])
    w<-e*g/(g+h)
    u<-e*h/(g+h)
    v<-TempData$NKN[i]-cd-ad*TempData$NKD[i]-bd*TempData$JPDH[i]-mu*w
    k<-abs(omegak+alphak*v*v+betak*k+gammak*TempData$JPDH[i])
    summ<-summ-log(2*3.1415926)+1/2*(log(g+h)+e*e/(g+h)+log(k)+v*v/k)
  }
  return (summ)
}
SignalModelMLE2A<-mle2(f2A,start=list(cf=Model2A@fit$coef[1],af=Model2A@fit$coef[2],bf=Model2A@fit$coef[3],omegag=Model2A@fit$coef[4],alphag=Model2A@fit$coef[5],betag=Model2A@fit$coef[6],gammag=Model2A@fit$coef[7],omegah=Model2A@fit$coef[4],alphah=Model2A@fit$coef[5],betah=Model2A@fit$coef[6],gammah=Model2A@fit$coef[7],cd=Model2B@fit$coef[1],ad=Model2B@fit$coef[2],bd=Model2B@fit$coef[3],mu=Model2B@fit$coef[4],omegak=Model2B@fit$coef[5],alphak=Model2B@fit$coef[6],betak=Model2B@fit$coef[7],gammak=Model2B@fit$coef[8]),"Nelder-Mead")
Result2A<- SignalModelMLE2A@coef
Result2A

Var<-solve(SignalModelMLE2A@details$hessian)
stderror2<-c()
for (i in 1:19){
  stderror2[i]<-sqrt(abs(Var[i,i]))
}
stderror2
Result2A/stderror2

##Signal Extraction Model Computation.
###S&P500->NIKKEI225
### Set initial value.
w1<-c()
u1<-c()
g1<-c()
h1<-c()
v1<-c()
k1<-c()
e1<-c()
x<-SignalModelMLE1A@coef
summ<-0
w1[1]<-0.7*TempData$JPResid[2]
u1[1]<-0.3*TempData$JPResid[2]
g1[1]<-10/3*TempData$JPVol[2]
h1[1]<-10/7*TempData$JPVol[2]
v1[1]<-TempData$USResid[1]
k1[1]<-TempData$USVol[1]
for (i in 2:length(TempData$Date))
{
  e1[i]<-TempData$NKD[i]-x[1]-x[2]*TempData$NKN[i-1]-x[3]*TempData$JPDH[i]
  var<-g1[i-1]*h1[i-1]/(g1[i-1]+h1[i-1])
  g1[i]<-abs(x[4]+x[6]*g1[i-1]+x[5]*(w1[i-1]*w1[i-1]+var)+x[7]*TempData$JPDH[i])
  h1[i]<-abs(x[8]+x[10]*h1[i-1]+x[9]*(u1[i-1]*u1[i-1]+var)+x[11]*TempData$JPDH[i])
  w1[i]<-e1[i]*g1[i]/(g1[i]+h1[i])
  u1[i]<-e1[i]*h1[i]/(g1[i]+h1[i])
  v1[i]<-TempData$SPN[i]-x[12]-x[13]*TempData$SPD[i-1]-x[14]*TempData$USDH[i]-x[15]*w1[i]
  k1[i]<-abs(x[16]+x[17]*v1[i]*v1[i]+x[18]*k1[i-1]+x[19]*TempData$USDH[i])
  summ<-summ-log(2*3.1415926)+1/2*(log(g1[i]+h1[i])+e1[i]*e1[i]/(g1[i]+h1[i])+log(k1[i])+v1[i]*v1[i]/k1[i])
}
AIC1A<-(2*19-2*(-1)*summ)/length(TempData$Date)
BIC1A<-(log(length(TempData$Date))*19-2*(-1)*summ)/length(TempData$Date)
Corr1<-g1/(g1+h1)*SignalModelMLE1A@coef[15]
TempData$NKDSPNSE<-Corr1

### Set initial value.
###NIKKEI225->S&P500
x<-SignalModelMLE2A@coef
summ2<-0
w2<-c()
u2<-c()
g2<-c()
h2<-c()
v2<-c()
k2<-c()
e2<-c()
w2[1]<-0.7*TempData$USResid[1]
u2[1]<-0.3*TempData$USResid[1]
g2[1]<-10/3*TempData$USVol[1]
h2[1]<-10/7*TempData$USVol[1]
v2[1]<-TempData$JPResid[2]
k2[1]<-TempData$JPVol[2]
for (i in 2:length(TempData$Date))
{
  e2[i]<-TempData$SPD[i]-x[1]-x[2]*TempData$SPN[i]-x[3]*TempData$USDH[i]
  var<-g2[i-1]*h2[i-1]/(g2[i-1]+h2[i-1])
  g2[i]<-abs(x[4]+x[6]*g2[i-1]+x[5]*(w2[i-1]*w2[i-1]+var)+x[7]*TempData$USDH[i])
  h2[i]<-abs(x[8]+x[10]*h2[i-1]+x[9]*(u2[i-1]*u2[i-1]+var)+x[11]*TempData$USDH[i])
  w2[i]<-e2[i]*g2[i]/(g2[i]+h2[i])
  u2[i]<-e2[i]*h2[i]/(g2[i]+h2[i])
  v2[i]<-TempData$NKN[i]-x[12]-x[13]*TempData$NKD[i]-x[14]*TempData$JPDH[i]-x[15]*w2[i]
  k2[i]<-abs(x[16]+x[17]*v2[i]*v2[i]+x[18]*k2[i-1]+x[19]*TempData$JPDH[i])
  summ2<-summ2-log(2*3.1415926)+1/2*(log(g2[i]+h2[i])+e2[i]*e2[i]/(g2[i]+h2[i])+log(k2[i])+v2[i]*v2[i]/k2[i])
}
AIC2A<-(2*19-2*(-1)*summ2)/length(TempData$Date)
BIC2A<-(log(length(TempData$Date))*19-2*(-1)*summ2)/length(TempData$Date)
Corr2<-g2/(g2+h2)*SignalModelMLE2A@coef[15]
TempData$SPDNKNSE<-Corr2

write.table(TempData,"Data1.csv",sep=",",row.names=FALSE,col.names=TRUE)


SPDNKN<-c()

###120 Days OLS Rolling Window
for (i in 120:length(TempData$Date)){
  End<-TempData$Date[i]
  RollinWindow<-TempData[TempData$Date>=End-119&TempData$Date<=End,]
  SPDNKN[i]<-cor(RollinWindow$NKN,RollinWindow$SPD)
}
NKDSPN<-c()
for (i in 120:length(TempData$Date)){
  End<-as.numeric(TempData$Date[i])
  RollinWindow<-TempData[TempData$Date>=End-119&TempData$Date<=End,]
  NKDSPN[i]<-cor(RollinWindow$NKD,RollinWindow$SPN)
}
TempData$NKDSPNSE<-Corr1
TempData$SPDNKNSE<-Corr2
TempData$NKDSPNOLS<-NKDSPN
TempData$SPDNKNOLS<-SPDNKN

###GARCH-Mean Model
###NIKKEI225->S&P500
####Step 1
Spec3A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(JPData$JPDH[1:length(JPData$JPDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(JPData$NKN[1:length(JPData$NKN)-1],JPData$JPDH[1:length(JPData$JPDH)-1]),include.mean = TRUE,archm=TRUE))
Model3A<- ugarchfit(Spec3A,JPData$NKD[2:length(JPData$NKD)])
show(Model3A)
JPData$JPResidGM[2:length(JPData$Date)]<-Model3A@fit$residuals
TempData<-merge(JPData,TempData)
####Step 2
Spec3B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$USDH[1:length(TempData$USDH)-1])),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$SPD[1:length(TempData$SPD)-1],TempData$USDH[1:length(TempData$USDH)-1],TempData$JPResid[1:length(TempData$JPResidGM)-1]),include.mean = TRUE,archm=TRUE))
Model3B <- ugarchfit(Spec3B,TempData$SPN[2:length(TempData$SPN)])
show(Model3B)
###S&P500->NIKKEI225
####Step 1
Spec4A<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(USData$USDH)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(USData$SPN,USData$USDH),include.mean = TRUE,archm=TRUE))
Model4A<- ugarchfit(Spec4A,USData$SPD[1:length(USData$SPD)])
show(Model4A)
USData$USResidGM<-Model4A@fit$residuals
TempData<-merge(USData,TempData)
####Step 2
TempData<-merge(JPData,USData)
TempData<-na.omit(TempData)
Spec4B<- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1),external.regressors=cbind(TempData$JPDH)),mean.model=list(armaOrder=c(0,0),external.regressors=cbind(TempData$NKD,TempData$JPDH,TempData$USResidGM),include.mean = TRUE,archm=TRUE))
Model4B <- ugarchfit(Spec4B,TempData$NKN)
show(Model4B)

