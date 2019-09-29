library(astsa)
### Raw Counts Extracted From Original Dataset-starting at April 1st 2015 due to missing data sets ###
total.dates=Full.Dates
length=length(modal_nyc_trips$count_total)

#####################################################################################################
NYC.Total.DLM=xts(NYC_Raw_Count_Total/1000000,order.by = total.dates)
num=length(NYC_Raw_Count_Total)

### EDA
par(mfrow=c(1,2))
hist(NYC.Total.DLM)
qqnorm(NYC.Total.DLM);qqline(NYC.Total.DLM)

### Make box-cox transform for lambda equal to 2 ### 

### Create a variable for trend as well ### 
t=1:train_len

### Create indicators for each day ### 
wed=rep(c(1,0,0,0,0,0,0),length=train_len)
thu=rep(c(0,1,0,0,0,0,0),length=train_len)
fri=rep(c(0,0,1,0,0,0,0),length=train_len)
sat=rep(c(0,0,0,1,0,0,0),length=train_len)
sun=rep(c(0,0,0,0,1,0,0),length=train_len)
mon=rep(c(0,0,0,0,0,1,0),length=train_len)
tue=rep(c(0,0,0,0,0,0,1),length=train_len)
seasonal.df=cbind(wed,thu,fri,sat,sun,mon,tue)

### Exogenous Predictors ###
peak.travel=nyc_peak$holiday_season[91:length(modal_nyc_trips$date)]
permits=nyc_events$event_count[91:length(modal_nyc_trips$date)]
major.holiday=nyc_holidays$major_holidays[91:length(modal_nyc_trips$date)]
precip=nyc_weather$PRCP_NYC[91:length(modal_nyc_trips$date)]

### Training Set  ###  
train.dates=seq(as.Date("2015-04-01"),length.out = train_len,by="days")
total.train=NYC.Total.DLM[1:train_len]
precip.train=precip[1:train_len]
holiday.train=major.holiday[1:train_len]
peak.train=peak.travel[1:train_len]
permits.train=permits[1:train_len]
trend=time(total.train)-mean(total.train)
exog.train=cbind("Precipitation"=precip.train,"Holiday"=holiday.train,"PeakTrav"=peak.train,"Permits"=permits.train)
### Holdout Data ###

pred.seq=(train_len+1):length(NYC.Total.DLM)
pred.dates=seq(as.Date("2017-04-07"),length.out = 85,by="days")
total.pred=xts(NYC.Total.DLM[pred.seq],order.by = pred.dates)
holiday.pred=major.holiday[pred.seq]
peak.pred=peak.travel[pred.seq]
precip.pred=precip[pred.seq]
permits.pred=permits[(train_len+1):822]
exog.pred=cbind(precip.pred,holiday.pred,peak.pred)

### Periodogram for Raw Data ###
par(mfrow=c(1,2))
NYC.Tot.Raw=mvspec(NYC.Total.DLM,log='no')
NYC.Tot.Ave=mvspec(NYC.Total.DLM,kernel = kernel('daniell',4),log='no')
weekly=7

### Static LM Model to Determine Initial parameter estimates ### 
static.total=lm(total.train~trend+seasonal.df+precip.train+peak.train+holiday.train+permits.train-1)
summary(static.total)
static.fitted=fitted.values(static.total)

### Plots for Static Model 

plot.xts(total.train)
lines(static.fitted,col = 'red')

par(mfrow=c(1,2))
acf(static.total$residuals)
pacf(static.total$residuals)

### Total Ridership DLM using Shumway and Stoffer Method ### 
### Initial Parameter Estimates ###
mu0.total=c(as.numeric(total.train[1]),rep(1,6))
varcov.total=vcov(static.total)
coefvar.total=diag(varcov.total)
sigma0.total=diag(coefvar.total, 7)
input.total=cbind(precip.train, holiday.train,peak.train) # taxi, citi) #nxr vector of exog inputs

A=array(cbind(1,1,0,0,0,0,0), dim=c(1,7,train_len)) # y_t = T_t + S_t + gamma*Inputs_t   

### Random Walk Estimate  ### 
Linn.Total=function(para){
  Phi = diag(0,7); Phi[1,1] = para[1]
  Phi[2,]=c(0,-1,-1,-1,-1,-1,-1); Phi[3,]=c(0,1,0,0,0,0,0); Phi[4,]=c(0,0,1,0,0,0,0); 
  Phi[5,]=c(0,0,0,1,0,0,0); Phi[6,]=c(0,0,0,0,1,0,0); Phi[7,]=c(0,0,0,0,0,1,0);
  cQ1 = para[2]; cQ2 = para[3]     # sqrt q11 and sqrt q22
  cQ=diag(0,7); cQ[1,1]=cQ1; cQ[2,2]=cQ2;
  cR = para[4]       # sqrt r11
  Ups = diag(0, 7,dim(input.total)[2])
  Ups[1,1] = para[5]; Ups[1,2]= para[6]; Ups[1,3]= para[7];#Ups[1,4]=para[8] 
  # Use Kfilter1 and Ksmooth1 for DLM with exogenous inputs
  kf = Kfilter1(train_len,total.train,A,mu0.total,sigma0.total,Phi,Ups,0,cQ,cR,input.total)
  return(kf$like)   # returns the log likelihood  
}

init.par.total=c(rep(3,7))

est.total = optim(init.par.total, Linn.Total, NULL, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1, maxit=2000))
SE.total = abs(sqrt(diag(solve(est.total$hessian))))
u1.total = cbind.data.frame(Estimate=est.total$par,"Standard Error"=SE.total)
rownames(u1.total)=c("Phi","SigW1","SigW2","SigV", "Precipitation", "Holidays", "Peak Travel")
print(u1.total)

# Kalman filter -Local Level Model 
Phi.total = diag(0,7); Phi.total[1,1] = est.total$par[1]
Phi.total[2,]=c(0,-1,-1,-1,-1,-1,-1); Phi.total[3,]=c(0,1,0,0,0,0,0); Phi.total[4,]=c(0,0,1,0,0,0,0); 
Phi.total[5,]=c(0,0,0,1,0,0,0); Phi.total[6,]=c(0,0,0,0,1,0,0); Phi.total[7,]=c(0,0,0,0,0,1,0);
cQ1 = est.total$par[2]; cQ2 = est.total$par[3]     # sqrt q11 and sqrt q22
cQ.total=diag(0,7); cQ.total[1,1]=cQ1; cQ.total[2,2]=cQ2;
cR.total = est.total$par[4]       # sqrt r11
Ups.total = diag(0, 7,dim(input.total)[2])
Ups.total[1,1] = est.total$par[5]; Ups.total[1,2]= est.total$par[6]; Ups.total[1,3]= est.total$par[7]#Ups[1,4]=est.total$par[8] 

# Use Kfilter1 and Ksmooth1 for DLM with exogenous inputs
kf.total = Kfilter1(train_len,total.train,A,mu0.total,sigma0.total,Phi.total,Ups.total,0,cQ.total,cR.total,input.total)
ks.total=Ksmooth1(train_len,total.train,A,mu0.total,sigma0.total,Phi.total,Ups.total,0,cQ.total,cR.total,input.total)

### Filtered Values-Training Set ###
# Filtered Values 
total.filter=xts(kf.total$xf[1,,]+kf.total$xf[2,,],order.by = train.dates,by='days')
# Filtered Trend
trend.filtered=xts(kf.total$xf[1,,],order.by = train.dates,by='days')
trendf.u=trend.filtered+3*sqrt(kf.total$Pf[1,1,]);trendf.l=trend.filtered-3*sqrt(kf.total$Pf[1,1,])
# Filtered Seasonal Component 
season.filtered=xts(kf.total$xf[2,,],order.by = train.dates,by='days')
seasonf.u=season.filtered+3*sqrt(kf.total$Pf[2,2,]);seasonf.l=season.filtered-3*sqrt(kf.total$Pf[2,2,])


###### Filtered data- ie fitted ####
par(mfrow=c(1,1))
plot.xts(total.filter*1000000,col = 'red',type = 'o',lty = 2,pch=16,main = "Filtered Values",
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(Raw_Count.df$TotRawCt,col = 'black',pch=3,type = "p")

par(mfrow=c(1,1))
plot.xts(trend.filtered*1000000,col = 'blue',type = 'o',lty = 1,main = "Filtered Random Walk Component",
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)


###### Model Diagnostics- BIC,Innovations Plot, ACF/PACF for Innovations
# Innovations
innov.total= xts(kf.total$innov[1,1,],order.by = train.dates,by='days')
#windows()

layout(matrix(c(1,1,2,2,
                3,3,4,4,
                5,5,5,5),3,4,byrow=T))
hist(innov.total,main = "Histogram of Innovations") # Note the tail behavior
qqnorm(innov.total);qqline(innov.total)
acf(innov.total,main="Innovation ACF");pacf(innov.total,main="Innovations PACF") # ACF and PACF do seem somewhat clean 
plot.xts(innov.total,main = "Total Count Innovations",
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3) 

# Future Forecasts-Total Count #
input.predict.total = cbind(precip,major.holiday, peak.travel)
n.ahead=85; y1.total = xts(append(as.numeric(total.train), rep(NA,n.ahead)),order.by = total.dates)
rmspe = rep(0,n.ahead); x00 = kf.total$xf[,,train_len]; P00 = kf.total$Pf[,,train_len]
Q.total=t(cQ.total)%*%cQ.total; R.total=t(cR.total)%*%(cR.total)

for (m in 1:n.ahead){ 
  xp = Phi.total%*%x00 +Ups.total%*%input.predict.total[train_len+m,]
  Pp = Phi.total%*%P00%*%t(Phi.total)+Q.total
  sig = matrix(A[,,m],1,7)%*%Pp%*%t(matrix(A[,,m],1,7))+R.total
  K = Pp%*%t(matrix(A[,,m],1,7))%*%(1/sig)
  x00 = xp;
  P00 = Pp-K%*%matrix(A[,,m],1,7)%*%Pp
  y1.total[train_len+m] = matrix(A[,,m],1,7)%*%xp; rmspe[m] = sqrt(sig)  
  
}   

upper=xts(y1.total[(train_len+1):(train_len+n.ahead)]+2*rmspe)
lower=xts(y1.total[(train_len+1):(train_len+n.ahead)]-2*rmspe)
y1.total=y1.total

### Forecast Plots
par(mfrow=c(1,1))
plot.xts(y1.total[738:822]*1000000,main = "Forecast-Shumway and Stoffer Model",xlab="Date",ylab="Total Count",
         col = 'blue',type = 'o',ylim=c(min(c(lower*1000000)),max(c(upper*1000000))),
         pch=5,yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(NYC.Total.DLM[738:822]*1000000,col="black",type = 'o',pch=3,lty=2)
lines(c(upper*1000000),col = 'red',lty=2)
lines(c(lower*1000000),col = 'red',lty=2)
addLegend(legend.loc = 'bottomleft',legend.names = c("Forecast","Observed","95% PI"),
          col = c('blue','black','red'),lty=c(1,2,2),pch=c(5,3,NA))

mape.total=mean(abs((NYC.Total.DLM[(train_len+1):822] - y1.total[(train_len+1):822]))/NYC.Total.DLM[(train_len+1):822])*100
mape.total



