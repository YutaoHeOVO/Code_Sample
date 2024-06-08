remove(list=ls())
library(readr)
TempData <- read_csv("Data.csv")
##Signal Extraction
###NIKKEI225->S&P500
f1A<-function(cf,af,bf,omegag,alphag,betag,gammag,omegah,alphah,betah,gammah,cd,ad,bd,mu,omegak,alphak,betak,gammak){
  #### Set initial values.
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
Res<-mle2(f1A,start=list(cf=0.000698,af=0.084006,bf=-0.000400,omegag=0.000006,alphag=0.1427,betag=0.812389,gammag=0.004,omegah=0.000006,alphah=0.1427,betah=0.812389,gammah=0.004,cd=0.000420,ad=0.006602,bd=-0.000304,mu=0.177848,omegak=0.000001,alphak=0.171783,betak=0.812108,gammak=0.000000),"Nelder-Mead")

f2A<-function(cf,af,bf,omegag,alphag,betag,gammag,omegah,alphah,betah,gammah,cd,ad,bd,mu,omegak,alphak,betak,gammak){
  ### Set initial values.
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
Res2<-mle2(f2A,start=list(cf=0.000384,af=0.029840,bf=0.000315,omegag=0.000000,alphag=0.161273,betag=0.806074,gammag=0.000007,omegah=0.000000,alphah=0.161273,betah=0.806074,gammah=0.000007,cd=0.000499,ad=-0.138772,bd=-0.000707,mu=0.512043,omegak=0.000006,alphak=0.152321,betak=0.792581,gammak=0.000000),"Nelder-Mead")
