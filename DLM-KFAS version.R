### DLM for Total Count-KFAS library ###
library(KFAS)
library(forecast)
library(xts)

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

### Regression Model to get initial parameters
init.model=lm(TotCount.Train~trend+Precip.Train+Holidays.Train+Peak.Travel.Train+
                tue[1:train_len]+wed[1:train_len]+thu[1:train_len]+fri[1:train_len]+sat[1:train_len]+sun[1:train_len])
summary(init.model)

init.variance=diag(vcov(init.model))
init.model.coef=coef(init.model)

#### Put Initial Estimates Into Matrices for Use in SSModel()
a1.trend=init.model.coef[1]
p1.trend=matrix(0,1,1);diag(p1.trend)=init.variance[1]

a1.regression=init.model.coef[3:5]
p1.regression=matrix(0,3,3);diag(p1.regression)=init.variance[3:5]

a1.seasonal=init.model.coef[6:11]
p1.seasonal=matrix(0,6,6);diag(p1.seasonal)=init.variance[6:11]

### Model Specification
#### Note that initial parameters for variance have a huge effect on how well the model fits:

TotCount.Model=SSModel(TotCount.Train~-1+
                         SSMregression(~Precip.Train+Holidays.Train+Peak.Travel.Train,data = train.df,Q=diag(NA,3),a1=a1.regression,P1=p1.regression)+
                         SSMtrend(1,Q=NA,a1=a1.trend,P1=p1.trend)+
                         SSMseasonal(period = 7,Q=NA,a1=a1.seasonal,P1=p1.seasonal),H=NA)

### Note that if we constrain state variance of seasonal component to zero along with 15 for our initial parameter estimates then our MAPE is lowest then we 

TotMLE=fitSSM(TotCount.Model,inits = rep(0,6),method='L-BFGS-B',hessian=T) 
TotCount.Model

Total.Output=KFS(TotMLE$model,smoothing = c("state","signal"), filtering =  c("state","signal","disturbance"))
print(Total.Output)



# Solve for SE of hyperparamter-note that KFAS uses log-cholesky so we exponentiate to get variances 

avarLog=solve(TotMLE$optim.out$hessian)
avar=diag(exp(TotMLE$optim.out$par)) %*% avarLog %*% diag(exp(TotMLE$optim.out$par))


signif((exp(TotMLE$optim.out$par)),3)
signif(sqrt(abs(diag(avar))),3)

# Variances 

TotMLE$model$H
TotMLE$model$Q

# Filtered States and SE
Filtered.States=colMeans(Total.Output$att[,1:4])
Filtered.SE=matrix(NA,nrow=737,ncol=10)
for(k in 1:10)
  for(i in 1:737){
  
    Filtered.SE[i,k]=Total.Output$Ptt[k,k,i]
  
  }
Filtered.States
sqrt(colMeans(Filtered.SE))


# Smoothed States and SE
Smoothed.States=colMeans(Total.Output$alphahat[,1:4])
Smoothed.SE=matrix(NA,nrow=737,ncol=10)
for(k in 1:10)
  for(i in 1:737){
    
    Smoothed.SE[i,k]=Total.Output$V[k,k,i]
    
  }




Smoothed.States
sqrt(colMeans(Smoothed.SE))


par(mfrow=c(2,1))
ts.plot(Total.Output$a[,1],main="Preciptation-Filtered")
ts.plot(Total.Output$alphahat[,1],main="Precipitation-Smoothed")

### Confidence Interval 
ci.total=xts(predict(TotMLE$model,interval = "confidence",level = 0.95,type = 'response',states=c("all","trend","seasonal")),order.by = Full.Dates[1:737])


signal.full=signal(Total.Output,filtered = T)
level.signal=signal(Total.Output,states = c("level"))
seasonal.signal=signal(Total.Output,states = c("seasonal"))
regression.signal=signal(Total.Output,states = c("regression"))

### residuals 
response.resids=Innovations=residuals(Total.Output)
windows()
par(mfrow=c(1,2))
acf(response.resids,xlab="",main="ACF-Innovations");pacf(response.resids,xlab="",main="PACF-Innovations")


### Prediction ###
Pred.Model=SSModel(rep(NA,85)~-1+SSMregression(~Precip.Pred+Holidays.Pred+Peak.Travel.Pred,data = pred.df,Q=TotMLE$model$Q[1:3,1:3,])+
                     SSMtrend(1,Q=list(TotMLE$model$Q[4,4,],TotMLE$model$Q[4,4,]))+
                     SSMseasonal(period = 7,Q=TotMLE$model$Q[5,5,]),H=TotMLE$model$H)

Total.Predict=xts(predict(TotMLE$model,newdata = Pred.Model,interval = "prediction",level = 0.95,type = 'response',states = 'all',filtered = T,se.fit = T),
                  order.by = Full.Dates[pred.seq])

mape.total.kfas=mean(abs((Raw_Count.df$TotRawCt[(train_len+1):822] - Total.Predict$fit))/Raw_Count.df$TotRawCt[(train_len+1):822])*100
mape.total.kfas

Total.Predict
#=======================================#
#======== PLOTTING =====================# 
#=======================================#

#windows()
ts.plot(Innovations,ylab="Innovations")
qqnorm(Innovations);qqline(Innovations)



par(mfrow=c(3,1))
ts.plot(level.signal$signal,ylab="Filtered Level")
ts.plot(seasonal.signal$signal,ylab="Filtered Seasonal")
ts.plot(regression.signal$signal,ylab="Filtered Regression Components")


Total.Forecast.Plot=plot.xts(Raw_Count.df$TotRawCt[pred.seq],col = 'black',type = 'o',pch=3,lty=2,
         ylim = c(min(Raw_Count.df$TotRawCt[pred.seq]-1000000),max(Total.Predict[,1]+500000)),main = "Total Count Forecast")
Total.Forecast.Plot=lines(as.xts(Total.Predict[,1]),col = 'blue',type = 'o',pch=16,lty=1)
Total.Forecast.Plot=addLegend(legend.loc = 'bottomleft',legend.names = c("Observed","Forecast"),
                              col = c("black","blue"),lty=c(2,1),pch=c(3,16))


par(mfrow=c(1,1))
Total.Forecast.Plot

