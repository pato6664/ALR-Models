### Forecasting- Script has both forecast components and forecast proportions backed out using alrInv  from 'compositions' library ###
### Returns a list containing fitted values for VARX(p) fit to ALR components###
"forecast.compositional"=function(mod_list,holdout,h,conf.lvl)
{
  forecast.list=list()  
  for(i in 1:length(mod_list))
  {
    forecast.list[[i]]=predict(mod_list[[i]],n.ahead =h,ci=conf.lvl,dumvar = holdout ) 
    names(forecast.list)[i]= paste(h,"Step Ahead",names(mod_list)[i])
    
  }  
  
  return(forecast.list)
  
}

### Returns the Absolute Percentage Error for each point forecast
# MAPE name is misleading since function does not actually calculate MAPE internally-use colMeans to find actual MAPE

"MAPE"=function(Fcst,Actual){
  
  
  if(nrow(Fcst)==nrow(Actual) & ncol(Fcst)==ncol(Actual)){
    
    MAPE.df=matrix(NA,nrow = nrow(Fcst),ncol = ncol(Fcst))
    
    for(i in 1:ncol(Fcst)){
      
      
      for(k in 1:nrow(Fcst)){
        
        MAPE.df[k,i]=abs((Actual[k,i] - Fcst[k,i])/Actual[k,i])*100  
        
      } 
        
        
      
    }
      
  }

  else{
    
    print("Dimensions of Forecast Data Frame and Actual Data Frame are Not Equal")
    
  }
  colnames(MAPE.df)=colnames(Actual)  
  return(MAPE.df)
}

### Returns the Absolute  Error for each point forecast
# MAE name is misleading since function does not actually calculate MAE internally-use colMeans to find actual MAE


"MAE"=function(Fcst,Actual){
  
  
  if(nrow(Fcst)==nrow(Actual) & ncol(Fcst)==ncol(Actual)){
    
    MAE.df=matrix(NA,nrow = nrow(Fcst),ncol = ncol(Fcst))
    
    for(i in 1:ncol(Fcst)){
      
      
      for(k in 1:nrow(Fcst)){
        
        MAE.df[k,i]=abs((Actual[k,i] - Fcst[k,i]))  
        
      } 
      
      
      
    }
    
  }
  
  else{
    
    print("Dimensions of Forecast Data Frame and Actual Data Frame are Not Equal")
    
  }
  colnames(MAE.df)=colnames(Actual)  
  return(MAE.df)
}



ALR.Forecast=forecast.compositional(mod_list=ALR.Model.List,holdout=Hold.DF[,4:15],h=85,conf.lvl=0.95)


### Forecast Dates
Fcst.Dates=seq(as.Date("2017-04-07"),length.out = 85,by='days')

### Data Frames for Each Component Forecast-Includes Prediction Intervals 
ALR.Order1.Fcst.TNC=xts(ALR.Forecast$`85 Step Ahead Fit for 1`$fcst$TNCComp,order.by = Fcst.Dates,by='days')
ALR.Order1.Fcst.Taxi=xts(ALR.Forecast$`85 Step Ahead Fit for 1`$fcst$TaxiComp,order.by = Fcst.Dates,by='days')
ALR.Order1.Fcst.Citi=xts(ALR.Forecast$`85 Step Ahead Fit for 1`$fcst$CitiComp,order.by = Fcst.Dates,by='days')

### Separate DF just for Point Predictions ###
ALRPointPred.df=cbind.data.frame("TNCComp"=ALR.Order1.Fcst.TNC$fcst,"TaxiComp"=ALR.Order1.Fcst.Taxi$fcst,"CitiComp"=ALR.Order1.Fcst.Citi$fcst)

### Separate DF for Intervals
ALRPred.L=cbind.data.frame("TNCComp"=ALR.Order1.Fcst.TNC$lower,"TaxiComp"=ALR.Order1.Fcst.Taxi$lower,"CitiComp"=ALR.Order1.Fcst.Citi$lower)
ALRPred.U=cbind.data.frame("TNCComp"=ALR.Order1.Fcst.TNC$upper,"TaxiComp"=ALR.Order1.Fcst.Taxi$upper,"CitiComp"=ALR.Order1.Fcst.Citi$upper)



### MAPE and MAE for ALR-VARX(1) Model 
ALR.MAPE.df=MAPE(ALRPointPred.df,component.df[(train_len+1):nrow(component.df),]); # View(ALR.MAPE.df)
ALR.MAE.df=MAE(ALRPointPred.df,component.df[(train_len+1):nrow(component.df),]); #View(ALR.MAE.df)
colMeans(ALR.MAPE.df)
colMeans(ALR.MAE.df)

### ALR Forecast Plots -TNC### 
#windows()
TNC.ALR.Plt=plot.xts(ALR.Order1.Fcst.TNC$fcst,col = 'blue',type = "o",
         lty=1,ylim=c(min(ALR.Order1.Fcst.TNC$lower),max(ALR.Order1.Fcst.TNC$upper)),
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3,main = "ALR Forecast-TNC")
TNC.ALR.Plt=lines(ALR.Order1.Fcst.TNC$upper,col = 'red',type = "l",lty = 2)
TNC.ALR.Plt=lines(ALR.Order1.Fcst.TNC$lower,col = 'red',type = "l",lty = 2)
TNC.ALR.Plt=lines(component.df$TNCComp[723:822],col = 'black',type = "p")

### ALR Forecast Plots- Taxi ### 
#windows()
Taxi.ALR.Plt=plot.xts(ALR.Order1.Fcst.Taxi$fcst,col = 'blue',type = "o",
            lty=1,ylim=c(min(ALR.Order1.Fcst.Taxi$lower),max(ALR.Order1.Fcst.Taxi$upper)),
            yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3, main = "ALR Forecast-Taxi")
Taxi.ALR.Plt=lines(ALR.Order1.Fcst.Taxi$upper,col = 'red',type = "l",lty = 2)
Taxi.ALR.Plt=lines(ALR.Order1.Fcst.Taxi$lower,col = 'red',type = "l",lty = 2)
Taxi.ALR.Plt=lines(component.df$TaxiComp[723:822],col = 'black',type = "p")

### ALR Forecast Plots- Taxi ### 
#windows()
par(mfrow=c(1,1))
Citi.ALR.Plt=plot.xts(ALR.Order1.Fcst.Citi$fcst,col = 'blue',type = "o", main = "ALR Forecast-Citi Bike",
            lty=1,ylim=c(min(ALR.Order1.Fcst.Citi$lower),max(ALR.Order1.Fcst.Citi$upper)),
            yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
Citi.ALR.Plt=lines(ALR.Order1.Fcst.Citi$upper,col = 'red',type = "l",lty = 2)
Citi.ALR.Plt=lines(ALR.Order1.Fcst.Citi$lower,col = 'red',type = "l",lty = 2)
Citi.ALR.Plt=lines(component.df$CitiComp[723:822],col = 'black',type = "p")

### Put all Forecast ALR Components on one plot
layout(matrix(c(1,1,1,
                2,2,2,
                3,3,3),byrow = T))
TNC.ALR.Plt
Taxi.ALR.Plt
Citi.ALR.Plt



### Forecast Proportions ###
Prop.PointFcst.df=xts(alrInv(ALRPointPred.df),order.by = Full.Dates[738:822]) # Forecast Proportions from ALR Inverse function
colnames(Prop.PointFcst.df)=colnames(prop.df)
#View(Prop.PointFcst.df)

Prop.PointFcst.L=alrInv(ALRPred.L)
Prop.PointFcst.U=alrInv(ALRPred.U)
#View(Prop.PointFcst.L)
#View(Prop.PointFcst.U)
colnames(Prop.PointFcst.L)=colnames(Prop.PointFcst.U)=colnames(Prop.PointFcst.df)=colnames(prop.df)

### Mean Absolute Error for Proportion Forecast 
Prop.Fcst.MAE=MAE(Prop.PointFcst.df,prop.df[(train_len+1):nrow(prop.df),])
colMeans(Prop.Fcst.MAE)


