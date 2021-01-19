library(readxl)
install.packages('read.csv')
M2 <- read.csv("D:\108-1\時間序列\final告/Taiwan_M2_Money_Supply.csv")
outbound <- read.csv("D:\108-1\時間序列\final/Taiwan_outbound.csv")
inbound <- read.csv("D:\108-1\時間序列\final/Taiwan_inbound.csv", encoding='big5')


#將資料轉成時間序列
ts.M2=ts(M2$Value, start=c(2000,1), frequency = 12)
ts.inbound=ts(inbound$來臺旅客人數, start=c(2000,1), frequency = 12)
ts.outbound=ts(outbound$國人出國人數, start=c(2000,1), frequency = 12)

#原始資料的時間序列
plot(ts.M2,main="M2 Money Supply",ylab="M2")
plot(ts.outbound,main="Outbound Passenger",ylab="Outbound Passenger")
points(time(ts.outbound),ts.outbound,col="red",pch=as.vector(season(ts.outbound)),cex=0.6)
plot(ts.inbound,main="Inbound Passenger",ylab="Inbound Passenger")
points(time(ts.inbound),ts.inbound,col="red",pch=as.vector(season(ts.inbound)),cex=0.6)




#######################################################################################
#Time Series

##1.
##M2
#數值太大 直接取log 
plot(log(ts.M2),main="Log of M2 Money Supply",ylab="M2") 
acf(log(ts.M2),main="ACF of Log of M2 Money Supply")
pacf(log(ts.M2),main="PACF of Log of M2 Money Supply")
eacf(log(ts.M2))
#檢定是否需差分
adf.test(log(ts.M2)) #需差分
kpss.test(log(ts.M2)) #需差分

#有向上的趨勢 取一階差分
plot(diff(log(ts.M2)),main="First Difference of Log of M2 Money Supply",ylab="M2")
acf(diff(log(ts.M2)),main="ACF of First Difference of Log of M2 Money Supply",lag.max = 80)
pacf(diff(log(ts.M2)),main="PACF of First Difference of Log of M2 Money Supply",lag.max = 80)
eacf(diff(log(ts.M2)))

#seasonal difference
plot(diff(diff(log(ts.M2)),12),main="First and Seasonal Differences of Log of M2 Money Supply",ylab="M2")
acf(diff(diff(log(ts.M2)),12),main="ACF of First and Seasonal Differences of Log of M2 Money Supply",lag.max = 80)
pacf(diff(diff(log(ts.M2)),12),main="PACF of First and Seasonal Differences of Log of M2 Money Supply",lag.max = 80)


#檢定是否再需差分
adf.test(diff(diff(log(ts.M2)),12)) #不需差分
kpss.test(diff(diff(log(ts.M2)),12)) #不需差分



#看ACF 配 SARIMA(0,1,0)∗(0,1,1)
arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(0,1,1),period=12),method="ML")
#choose model SARIMA(0,1,0)*(0,1,1)
#Parameter Estimation
arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(0,1,1),period=12),method="ML")
model_M2=arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(0,1,1),period=12),method="ML")
model_M2


#detect Outliers
detectAO(model_M2)
detectIO(model_M2) #detect 13 outlier  

#但 13 outlier 不顯著
arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(0,1,1),period=12),xreg=data.frame(AO13=1*(seq(log(ts.outbound))==13)))
arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(0,1,1),period=12),io=c(13))

#Model Diagnostics 
res_M2=residuals(model_M2)
plot(res_M2,main="Residuals of M2 model",ylab="residuals")
text(time(res_M2)[13],res_M2[13],labels=13,col="blue",cex=0.5)

##有丟前面13筆殘差
hist(res_M2[-c(1:13)],main="Histogram of residuals of M2 model",xlab="residuals[-c(1:13)]")
qqnorm(res_M2[-c(1:13)],main="Normal Q-Q Plot of residuals of M2 model"); qqline(res_M2[-c(1:13)])

t.test(res_M2[-c(1:13)]) #通過
shapiro.test(res_M2[-c(1:13)]) #通過 

acf(res_M2[-c(1:13)],lag.max = 50,main="ACF of residuals[-c(1:13)] of M2 model") 
Box.test(res_M2[-c(1:13 )],lag=6,type="Ljung")
Box.test(res_M2[-c(1:13)],lag=10,type="Ljung") #沒通過!!!!!
Box.test(res_M2[-c(1:13)],lag=16,type="Ljung") #沒通過!!!!!

##沒有丟前面13筆殘差
hist(res_M2) #沒通過 
qqnorm(res_M2); qqline(res_M2)  #沒通過 

t.test(res_M2) #通過
shapiro.test(res_M2) #沒通過 

acf(as.vector(res_M2),lag.max = 50,main="ACF of residuals of M2 model") 
Box.test(res_M2,lag=38,type="Ljung") #通過

#候選模型
#看PACF 配 SARIMA(0,1,0)∗(1,1,0)
m=arima(log(ts.M2), order = c(0,1,0),seasonal = list(order=c(4,1,0),period=12),method="ML")


detectAO(m)
detectIO(m) #detect 13 outlier  但不顯著
res1=residuals(m)
t.test(res1[-c(1:13)]) #通過
shapiro.test(res1[-c(1:13)]) #沒通過 
acf(as.vector(res1[-c(1:13)]),lag.max = 50) #沒通過
Box.test(res1[-c(1:13)],lag=6,type="Ljung") 
Box.test(res1[-c(1:13)],lag=16,type="Ljung") 
Box.test(res1[-c(1:13)],lag=17,type="Ljung") 
Box.test(res1[-c(1:13)],lag=38,type="Ljung") 


##2.
##Outbound Passenger 
#數值太大 直接取log 
plot(log(ts.outbound),main="Log of Outbound Passenger",ylab="Outbound Passenger") 
acf(log(ts.outbound),main="ACF of Log of Outbound Passenger",ylab="Outbound Passenger") 
pacf(log(ts.outbound),main="PACF of Log of Outbound Passenger",ylab="Outbound Passenger") 
#檢定是否需差分
adf.test(log(ts.outbound)) #不需差分
kpss.test(log(ts.outbound)) #需差分

#有向上的趨勢 取一階差分
plot(diff(log(ts.outbound)),main="First Difference of Log of Outbound Passenger",ylab="Outbound Passenger")
acf(diff(log(ts.outbound)),main="ACF of First Difference of Log of Outbound Passenger",lag.max = 80)
pacf(diff(log(ts.outbound)),main="PACF of First Difference of Log of Outbound Passenger",lag.max = 80)

#seasonal difference
plot(diff(diff(log(ts.outbound)),12),main="First and Seasonal Differences of Log of Outbound Passenger",ylab="Outbound Passenger")
acf(diff(diff(log(ts.outbound)),12),main="ACF of First and Seasonal Differences of Log of Outbound Passenger",lag.max = 80)
pacf(diff(diff(log(ts.outbound)),12),main="PACF of First and Seasonal Differences of Log of Outbound Passenger",lag.max = 80)

#檢定是否再需差分
adf.test(diff(diff(log(ts.outbound)),12)) #不需差分
kpss.test(diff(diff(log(ts.outbound)),12)) #不需差分

#Intervention Analysis
plot(log(ts.outbound),main="Log of Outbound Passenger",ylab="Outbound Passenger") 
text(time(log(ts.outbound)),log(ts.outbound),col="blue",cex=0.5,label=seq(1,length(log(ts.outbound))))
plot(log(ts.outbound),main="Log of Outbound Passenger",ylab="Outbound Passenger") 
text(time(log(ts.outbound))[seq(39,43)],log(ts.outbound)[seq(39,43)],col="red",cex=0.5,label=c("Mar03","Apr03","May03","Jun03","Jul03"))
text(time(log(ts.outbound))[c(50)],log(ts.outbound)[c(50)],col="blue",cex=0.5,label=c("Feb04"))


out_intervention_plot=window(log(ts.outbound),star=c(2002,1),end=c(2004,12))
plot(out_intervention_plot,main="Log of Outbound Passenger in 2002~2004",ylab="Outbound Passenger") 
text(time(log(ts.outbound))[seq(38,43)],log(ts.outbound)[seq(38,43)],col="red",cex=0.8,label=c("Feb03","Mar03","Apr03","May03","Jun03","Jul03"))
text(time(log(ts.outbound))[c(50)],log(ts.outbound)[c(50)],col="blue",cex=0.8,label=c("Feb04"))


#看PACF 配 SARIMA(3,1,0)∗(2,1,0)
arima(log(ts.outbound),order=c(2,1,0),seasonal = list(order=c(2,1,0),period=12),xtransf=data.frame(I39=1*(seq(log(ts.outbound))==39))
      ,transfer=list(c(1,2)) ,method = c("ML"),io=c(43))

#看ACF 配 SARIMA(0,1,3)∗(0,1,1) 但ma2 ma3 不顯著 
arima(log(ts.outbound),order=c(0,1,3),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.outbound))==39))
      ,transfer=list(c(1,2)) ,method = c("ML"))
arima(log(ts.outbound),order=c(0,1,2),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.outbound))==39))
      ,transfer=list(c(1,2)) ,method = c("ML"))

model_outbound_intervention=arima(log(ts.outbound),order=c(0,1,1),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.outbound))==39))
      ,transfer=list(c(1,2)) ,method = c("ML"))
detectAO(model_outbound_intervention)
detectIO(model_outbound_intervention)

#SARIMA(0,1,1)(0,1,1) 在Ｔ=39 配ＡRMA(1,2) 且50 是Ao 43是io
model_outbound=arima(log(ts.outbound),order=c(0,1,1),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.outbound))==39))
                     ,transfer=list(c(1,2)) ,method = c("ML"),xreg=data.frame(AO50=1*(seq(log(ts.outbound))==50)),io=c(43))
model_outbound
#detect Outliers
detectAO(model_outbound)
detectIO(model_outbound)
#Model Diagnostics 
res_outbound=residuals(model_outbound)
plot(res_outbound,main="Residuals of Outbound Passenger model",ylab="residuals")

##有丟前面13筆殘差
hist(res_outbound[-c(1:13)],main="Histogram of residuals of \nOutbound Passenger model",xlab="residuals[-c(1:13)]")
qqnorm(res_outbound[-c(1:13)],main="Normal Q-Q Plot of residuals of \nOutbound Passenger model"); qqline(res_outbound[-c(1:13)])

t.test(res_outbound[-c(1:13)]) #通過
shapiro.test(res_outbound[-c(1:13)]) #通過 

acf(res_outbound[-c(1:13)],lag.max =50,main="ACF of residuals of \nOutbound Passenger model") #通過
Box.test(res_outbound[-c(1:13 )],lag=11,type="Ljung") 
Box.test(res_outbound[-c(1:13 )],lag=36,type="Ljung")
Box.test(res_outbound[-c(1:13 )],lag=41,type="Ljung")



##3.
##Inbound Passenger 
#數值太大 直接取log 
plot(log(ts.inbound),main="Log of Inbound Passenger",ylab="Inbound Passenger") 
acf(log(ts.inbound),main="ACF of Log of Inbound Passenger",ylab="Inbound Passenger") 
pacf(log(ts.inbound),main="PACF of Log of Inbound Passenger",ylab="Inbound Passenger") 
#檢定是否需差分
adf.test(log(ts.inbound)) #不需差分
kpss.test(log(ts.inbound)) #需差分

#有向上的趨勢 取一階差分
plot(diff(log(ts.inbound)),main="First Difference of Log of Inbound Passenger",ylab="Inbound Passenger")
acf(diff(log(ts.inbound)),main="ACF of First Difference of Log of Inbound Passenger",lag.max = 80)
pacf(diff(log(ts.inbound)),main="PACF of First Difference of Log of Inbound Passenger",lag.max = 80)

#seasonal difference
plot(diff(diff(log(ts.inbound)),12),main="First and Seasonal Differences of Log of Inbound Passenger",ylab="Inbound Passenger")
acf(diff(diff(log(ts.inbound)),12),main="ACF of First and Seasonal Differences of Log of Inbound Passenger",lag.max = 80)
pacf(diff(diff(log(ts.inbound)),12),main="PACF of First and Seasonal Differences of Log of Inbound Passenger",lag.max = 80)


#檢定是否再需差分
adf.test(diff(diff(log(ts.inbound)),12)) #不需差分
kpss.test(diff(diff(log(ts.inbound)),12)) #不需差分



#Intervention Analysis
plot(log(ts.inbound),main="Log of Inbound Passenger",ylab="Inbound Passenger") 
text(time(log(ts.inbound)),log(ts.inbound),col="blue",cex=0.5,label=seq(1,length(log(ts.outbound))))
plot(log(ts.inbound),main="Log of Inbound Passenger",ylab="Inbound Passenger") 

in_intervention_plot=window(log(ts.inbound),star=c(2002,1),end=c(2009,12))
plot(in_intervention_plot,main="Log of Inbound Passenger in 2002~2009",ylab="Inbound Passenger") 
text(time(log(ts.inbound))[seq(39,45)],log(ts.inbound)[seq(39,45)],col="red",cex=0.8,label=c("03Mar","03Apr","03May","03Jun","03Aug","03Sep","03Oct"))
text(time(log(ts.inbound))[c(112)],log(ts.inbound)[c(112)],col="blue",cex=0.8,label=c("Apr09"))



#看ACF 配 SARIMA(0,1,3)∗(0,1,1) 但 ma3 不顯著 
arima(log(ts.inbound),order=c(0,1,3),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.inbound))==39))  
         ,transfer=list(c(1,3)) ,method = c("ML"))

model_in_intervention=arima(log(ts.inbound),order=c(0,1,2),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.inbound))==39))  
                            ,transfer=list(c(1,3)) ,method = c("ML"))
#detect Outliers
detectAO(model_in_intervention) 
detectIO(model_in_intervention) #detect 112 outlier



#choose model SARIMA(0,1,2)*(0,1,1) 在Ｔ=39 配ＡRMA(1,3) 且112 是io
#Parameter Estimation
model_inbound=arima(log(ts.inbound),order=c(0,1,2),seasonal = list(order=c(0,1,1),period=12),xtransf=data.frame(I39=1*(seq(log(ts.inbound))==39))  
                    ,transfer=list(c(1,3)) ,method = c("ML"),io=c(112))
model_inbound


detectIO(model_inbound)
detectAO(model_inbound)
#Model Diagnostics 
res_inbound=residuals(model_inbound)
plot(res_inbound,main="Residuals of Inbound Passenger model",ylab="residuals")
##有丟前面13筆殘差
hist(res_inbound[-c(1:13)],main="Histogram of residuals of \nInbound Passenger model",xlab="residuals[-c(1:13)]")
qqnorm(res_inbound[-c(1:13)],main="Normal Q-Q Plot of residuals of \nInbound Passenger model"); qqline(res_outbound[-c(1:13)])

t.test(res_inbound[-c(1:13)]) #通過
shapiro.test(res_inbound[-c(1:13)]) #沒通過 

acf(res_inbound[-c(1:13)],lag.max =50) #通過
Box.test(res_inbound[-c(1:13 )],lag=10,type="Ljung") 
Box.test(res_inbound[-c(1:13 )],lag=23,type="Ljung")







#correlation 

plot(ts.M2,ts.inbound)
plot(ts.M2,ts.outbound)
cor(ts.M2,ts.inbound)
cor(ts.M2,ts.outbound)

#配線性迴歸
summary(lm(ts.outbound~ts.M2))
summary(lm(ts.inbound~ts.M2))





plot(res_M2[-c(1:13)],res_inbound[-c(1:13)],)
plot(res_M2[-c(1:13)],res_outbound[-c(1:13)])

#M2 & Outbound Passenger
ccf(res_M2[-c(1:13)],res_outbound[-c(1:13)],main="M2 & Outbound Passenger",ylab='CCF',lag.max =36)


#0
round(cor(res_M2[14:(238-0)],res_outbound[(14+0):238]),4)
summary(lm(res_M2[14:(238-0)]~res_outbound[(14+0):238]))

#lag  -32
round(cor(res_M2[14:(238-32)],res_outbound[(14+32):238]),4)
summary(lm(res_M2[14:(238-32)]~res_outbound[(14+32):238]))
#lag  -24
round(cor(res_M2[14:(238-24)],res_outbound[(14+24):238]),4)
summary(lm(res_M2[14:(238-24)]~res_outbound[(14+24):238]))
#lag  -9
round(cor(res_M2[14:(238-9)],res_outbound[(14+9):238]),4)
summary(lm(res_M2[14:(238-9)]~res_outbound[(14+9):238]))
#lag  1
round(cor(res_outbound[14:(238-1)],res_M2[(14+1):238]),4)
summary(lm(res_outbound[14:(238-1)]~res_M2[(14+1):238]))
#lag  11
round(cor(res_outbound[14:(238-11)],res_M2[(14+11):238]),4)
summary(lm(res_outbound[14:(238-11)]~res_M2[(14+11):238]))
#lag  17
round(cor(res_outbound[14:(238-17)],res_M2[(14+17):238]),4)
summary(lm(res_outbound[14:(238-17)]~res_M2[(14+17):238]))






#M2 & Inbound Passenger
ccf(res_M2[-c(1:13)],res_inbound[-c(1:13)],main="M2 & Inbound Passenger",ylab='CCF',lag.max =36) #-12 

#lag    0
round(cor(res_M2[14:(238-0)],res_inbound[(14+0):238]),4)
summary(lm(res_M2[14:(238-0)]~res_inbound[(14+0):238]))
#lag  -27
round(cor(res_M2[14:(238-27)],res_inbound[(14+27):238]),4)
summary(lm(res_M2[14:(238-27)]~res_inbound[(14+27):238]))
#lag  -14
round(cor(res_M2[14:(238-14)],res_inbound[(14+14):238]),4)
summary(lm(res_M2[14:(238-14)]~res_inbound[(14+14):238]))
#lag  -12
round(cor(res_M2[14:(238-12)],res_inbound[(14+12):238]),4)
summary(lm(res_M2[14:(238-12)]~res_inbound[(14+12):238]))
#lag  10
round(cor(res_inbound[14:(238-10)],res_M2[(14+10):238]),4)
summary(lm(res_M2[14:(238-10)]~res_inbound[(14+10):238]))
#lag  17
round(cor(res_inbound[14:(238-17)],res_M2[(14+17):238]),4)
summary(lm(res_M2[14:(238-17)]~res_inbound[(14+17):238]))

ccf(diff(diff(log(ts.M2)),12),diff(diff(log(ts.inbound)),12))
ccf(diff(diff(log(ts.M2)),12),diff(diff(log(ts.outbound)),12))










