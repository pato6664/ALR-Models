### Count Forecasting ###

### Predictions Using Shumway and Stoffer
#TNC.Count.Pred=xts(Prop.PointFcst.df[,1]*y1.total[738:822]*1000000,order.by=Full.Dates[738:822],by='days')
#Taxi.Count.Pred=xts(Prop.PointFcst.df[,2]*y1.total[738:822]*1000000,order.by=Full.Dates[738:822],by='days')
#Citi.Count.Pred=xts(Prop.PointFcst.df[,3]*y1.total[738:822]*1000000,order.by=Full.Dates[738:822],by='days')
#Subway.Count.Pred=xts(Prop.PointFcst.df[,4]*y1.total[738:822]*1000000,order.by=Full.Dates[738:822],by='days')


### Predictions from KFAS 
TNC.Count.Pred=xts(Prop.PointFcst.df[,1]*Total.Predict[,1],order.by=Full.Dates[738:822],by='days')
Taxi.Count.Pred=xts(Prop.PointFcst.df[,2]*Total.Predict[,1],order.by=Full.Dates[738:822],by='days')
Citi.Count.Pred=xts(Prop.PointFcst.df[,3]*Total.Predict[,1],order.by=Full.Dates[738:822],by='days')
Subway.Count.Pred=xts(Prop.PointFcst.df[,4]*Total.Predict[,1],order.by=Full.Dates[738:822],by='days')


### Predictions from DLM library
#TNC.Count.Pred=xts(Prop.PointFcst.df[,1]*Total.Predict$f[738:822],order.by=Full.Dates[738:822],by='days')
#Taxi.Count.Pred=xts(Prop.PointFcst.df[,2]*Total.Predict$f[738:822],order.by=Full.Dates[738:822],by='days')
#Citi.Count.Pred=xts(Prop.PointFcst.df[,3]*Total.Predict$f[738:822],order.by=Full.Dates[738:822],by='days')
#Subway.Count.Pred=xts(Prop.PointFcst.df[,4]*Total.Predict$f[738:822],order.by=Full.Dates[738:822],by='days')



### Forecast Error ###
MAE.Fcst.Ct=MAE(cbind(TNC.Count.Pred,Taxi.Count.Pred,Citi.Count.Pred,Subway.Count.Pred),Raw_Count.df[738:822,1:4])
MAPE.Fcst.Ct=MAPE(cbind(TNC.Count.Pred,Taxi.Count.Pred,Citi.Count.Pred,Subway.Count.Pred),Raw_Count.df[738:822,1:4])

MAE.Fcst.Ct=colMeans(MAE.Fcst.Ct);MAE.Fcst.Ct
MAPE.Fcst.Ct=colMeans(MAPE.Fcst.Ct);
#rbind(MAPE.Fcst.Ct,mape.total.kfas)


MAE.Fcst.Ct
MAPE.Fcst.Ct
#mape.total.kfas

mean(MAPE.Fcst.Ct)
mean(MAE.Fcst.Ct)

### Forecast Plots ### 
### TNC

TNC.Count.Forecast.Plot=plot.xts(Raw_Count.df$TNCCt[738:822],col = 'black',type = 'o',pch=3,lty = 3,
                            ylim=c(min(Raw_Count.df$TNCCt[738:822]),max(TNC.Count.Pred+10000)),
                            main = "TNC Count Forecasts" ,yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
TNC.Count.Forecast.Plot=lines(TNC.Count.Pred,col = 'blue',type = 'o',pch=16,lty = 1)
#TNC.Count.Forecast.Plot=addLegend(legend.names = c("Observed","Forecast"),
                           # col = c('black','blue'),lty=c(3,1),pch=c(3,16))
TNC.Count.Forecast.Plot
### Taxi
Taxi.Count.Forecast.Plot=plot.xts(Raw_Count.df$TaxiCt[738:822],col = 'black',type = 'o',pch=3,lty = 3,
                                    ylim=c(min(Taxi.Count.Pred),max(Taxi.Count.Pred)+10000),
                                    yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3,
                                    main = "Taxi Count Forecasts")

Taxi.Count.Forecast.Plot=lines(Taxi.Count.Pred,col = 'blue',type = 'o',pch=16,lty = 1)

#Taxi.Count.Forecast.Plot=addLegend(legend.names = c("Observed","Forecast"),
                           #   col = c('black','blue'),lty=c(3,1),pch=c(3,16))
Taxi.Count.Forecast.Plot
### Citibike
Citibike.Count.Forecast.Plot=plot.xts(Raw_Count.df$CitiCt[738:822],col = 'black',type = 'o',pch=3,lty = 3,
         ylim=c(min(Raw_Count.df$CitiCt[738:822])-5000,max(Raw_Count.df$CitiCt[738:822]+10000)),
         main = "Citibike Count Forecasts",yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)

Citibike.Count.Forecast.Plot=lines(Citi.Count.Pred,col = 'blue',type = 'o',pch=16,lty = 1)

#Citibike.Count.Forecast.Plot=addLegend(legend.names = c("Observed","Forecast"),
        #  col = c('black','blue'),lty=c(3,1),pch=c(3,16))

Citibike.Count.Forecast.Plot

### Subway
Subway.Count.Forecast.Plot=plot.xts(Raw_Count.df$SubwayCt[738:822],col = 'black',type = 'o',pch=3,lty = 3,
                              ylim=c(min(Raw_Count.df$TNCCt[738:822]),max(Subway.Count.Pred+500000)),
                              main = "Subway Count Forecasts",yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)

Subway.Count.Forecast.Plot=lines(Subway.Count.Pred,col = 'blue',type = 'o',pch=16,lty = 1)

#Subway.Count.Forecast.Plot=addLegend(legend.names = c("Observed","Forecast"),
                             # col = c('black','blue'),lty=c(3,1),pch=c(3,16))

Subway.Count.Forecast.Plot
### Stack Plot of Modal Count Forecasts

layout(matrix(c(1,1,1,
                2,2,2,
                3,3,3,
                4,4,4),byrow = T))
TNC.Count.Forecast.Plot
Taxi.Count.Forecast.Plot
Citibike.Count.Forecast.Plot
Subway.Count.Forecast.Plot

### Stack Plots of Forecasts for:Total Count,TNC Count,and TNC Proportion
TNC.Stack1=plot.xts(TNC.Count.Pred,main = "TNC Forecast Count",
             col='blue',type = 'o',pch=16,lty=1,ylim=c(min(Raw_Count.df$TNCCt[738:822]),max(TNC.Count.Pred)))
TNC.Stack1=lines(Raw_Count.df$TNCCt[738:822],col = 'blue',type = 'l',lty = 2)
TNC.Stack2=plot.xts(as.xts(Total.Predict$fit),main="Total Count Forecast",
                col='black',type = 'o',pch=3,lty=2,ylim=c(min(Total.Predict$lwr),max(Total.Predict$upr+500000)))
TNC.Stack2=lines(Raw_Count.df$TotRawCt[738:822])
TNC.Stack3=plot.xts(Prop.PointFcst.df[,1],main = "TNC Forecast Proportion",
              col='red',type = 'o',pch=5,lty=2,ylim=c(0.04,.16))
TNC.Stack3=lines(Raw_Count.df$TNCCt[738:822]/Raw_Count.df$TotRawCt[738:822])

windows()
layout(matrix(c(1,1,1,
              2,2,2,
              3,3,3),byrow = T))
TNC.Stack1
TNC.Stack2
TNC.Stack3

### Stack Plots of Forecasts for:Total Count,Taxi Count,and Taxi Proportion
Taxi.Stack1=plot.xts(Taxi.Count.Pred,main = "Taxi Forecast Count",
             col='blue',type = 'o',pch=16,lty=1,ylim=c(min(Raw_Count.df$TaxiCt[738:822]),max(Taxi.Count.Pred)))
Taxi.Stack1=lines(Raw_Count.df$TaxiCt[738:822],col = 'purple',type = 'o',lty = 1,pch=3)




layout(matrix(c(1,1,1,
                2,2,2,
                3,3,3),byrow = T))
Taxi.Stack1


#plot(y1.total[738:822]*1000000,main="Total Count Forecast",
#     col='black',type = 'o',pch=3,lty=2)
#plot(Prop.PointFcst.df$TaxiProp,main = "Taxi Forecast Proportion",
#     col='red',type = 'o',pch=5,lty=2)



