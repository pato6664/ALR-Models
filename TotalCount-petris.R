# Using the DLM Package #
library(dlm)

# Data set up 
TotCount.Train=as.numeric(Raw_Count.df$TotRawCt[1:train_len])
Precip.Train=as.numeric(exog.df$Precip[1:train_len])
Holidays.Train=as.numeric(exog.df$Holidays[1:train_len])
Peak.Travel.Train=as.numeric(exog.df$PeakTrav[1:train_len])

train.df=cbind.data.frame(Precip.Train,Holidays.Train,Peak.Travel.Train)
#### Preciction Data Set
pred.seq=(train_len+1):length(Raw_Count.df$TotRawCt)
TotCount.Pred=as.numeric(Raw_Count.df$TotRawCt)
Precip.Pred=as.numeric(exog.df$Precip[pred.seq])
Holidays.Pred=as.numeric(exog.df$Holidays[pred.seq])
Peak.Travel.Pred=as.numeric(exog.df$PeakTrav[pred.seq])
pred.df=cbind.data.frame(Precip.Pred,Holidays.Pred,Peak.Travel.Pred)
trend=1:length(TotCount.Train)


# Exogenous Predictors
exo.dlm=cbind.data.frame("Precipitation"=exog.df$Precip,
                         "Holiday"=exog.df$Holidays,
                         "PeakTravel"=exog.df$Holidays)


# DLM Setup for Reference-Local Level Trend, Weekly Seasonality, Predictors for Precipitation, Holidays, and Peak Travel Season

#loc.lvl.weekly=dlmModARMA(ar=1)+dlmModSeas(7)+dlmModReg(X=exo.dlm,addInt = F)
#loc.lvl.weekly$W
#loc.lvl.weekly$V
#loc.lvl.weekly$GG

# Local Level with Weekly Seasonality 
buildDLM=function(psi){
  
  mod=dlmModPoly(1)+dlmModSeas(frequency = 7)+dlmModReg(X=exo.dlm,addInt = F) # Fixed regression components
  mod$V=exp(psi[1])
  mod$W[1,1]=exp(psi[2])
  mod$W[2,2]=0
  mod$W[8,8]=0
  mod$W[9,9]=0
  mod$W[10,10]=0
  
  innov=dlmModARMA(ar=psi[3],sigma2 = exp(psi[4]))
  
  return(mod+innov) 
}

TotalMLE=dlmMLE(log(TotCount.Train),c(rep(1,2),0.5,1),build=buildDLM,hessian=T,control=list(trace = 1, REPORT=1,maxit = 250))


avarLog=solve(TotalMLE$hessian)
avar=diag(exp(TotalMLE$par)) %*% avarLog %*% diag(exp(TotalMLE$par))
sqrt(abs(diag(avar)))

TotalMLE$value
exp(TotalMLE$par)

TotalDLM=buildDLM((TotalMLE$par))
Total.Filter=dlmFilter(TotCount.Train,TotalDLM)
Total.Smooth=dlmSmooth(TotCount.Train,TotalDLM)
Total.Res=resid(Total.Filter)
Total.Innovations=Total.Res$res
GG(TotalDLM)
W(TotalDLM)
V(TotalDLM)

# Model Diagnostics
# Filtered Values vs Actual 
windows()
plot(TotCount.Train,col='black',type='l')
lines(Total.Filter$f,col='red',type='l',lty=2)


# Residuals Diagnostics
windows()
par(mfrow=c(1,2))
hist(Total.Innovations);qqnorm(Total.Innovations);qqline(Total.Innovations)

# ACF and PACF
windows()
par(mfrow=c(1,2))
acf(Total.Innovations);pacf(Total.Innovations)

# Forecast for model without dynamic regression components 
#total.fcst=dlmForecast(Total.Filter,nAhead = 85)

#windows()
#plot(TotCount.Pred[738:822],col='black',type='o')
#lines(total.fcst$f,col='red',type='o')

#mape=mean(abs((Raw_Count.df$TotRawCt[(train_len+1):822] - total.fcst$f))/Raw_Count.df$TotRawCt[(train_len+1):822])*100
#mape


# Prediction Using Kalman Filter 
Pred=append(TotCount.Train,rep(NA,85))
Total.Predict=dlmFilter(Pred,TotalDLM)
length(Total.Predict$f)

windows()
plot(TotCount.Pred[738:822],col='black',type='o')
lines(Total.Predict$f[738:822],col='red',type='o')

mape.petris=mean(abs((Raw_Count.df$TotRawCt[(train_len+1):822] - Total.Predict$f[738:822]))/Raw_Count.df$TotRawCt[(train_len+1):822])*100
mape.petris
