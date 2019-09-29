#### Extraction of Residuals and Fitted Values for Training Set ###
# This script also has diagnostics plots located near the bottom of page #

### Returns a list containing fitted values for VARX(p) ###
"fitted.extract"=function(mod_list)
{
  fitted.list=list()  
  for(i in 1:length(mod_list))
  {
    fitted.list[[i]]=fitted(mod_list[[i]])  
    names(fitted.list)[i]= paste("Fitted Vals",names(mod_list)[i])
    
  }  
  
  return(fitted.list)
  
}

### Returns a list containing the residuals for each model contained within a list of VARX models ###
resid.extract=function(model_list)
{
  resid.list=list()  
  for(i in 1:length(model_list))
  {
    resid.list[[i]]=residuals(model_list[[i]])  
    names(resid.list)[i]= paste("Residuals for",names(model_list)[i])
    
  }  
  
  return(resid.list)
  
}

### ALR Residuals ### 
Full.ALR.Res=resid.extract(ALR.Model.List)
### ALR Fitted Components ###
Full.ALR.Fit=fitted.extract(ALR.Model.List)

###
model.dates=seq(as.Date("2015-04-02"),length.out = train_len-1,by="days")

ALR.Residuals.Ord1=xts(Full.ALR.Res$`Residuals for Fit for 1`,order.by = model.dates)
colnames(ALR.Residuals.Ord1)=colnames(component.df)

#### Residuals Diagnostic Plots ###
plot.xts(ALR.Residuals.Ord1$TNCComp,col=Color.Scheme[1]) #TNC
plot.xts(ALR.Residuals.Ord1$TaxiComp,col=Color.Scheme[2]) #Taxi
plot.xts(ALR.Residuals.Ord1$CitiComp,col=Color.Scheme[3]) #Citi


### Overlay of TNC and Taxi Residuals ###
windows()
plot.xts(ALR.Residuals.Ord1$TNCComp,col=Color.Scheme[1],type = "o",lty = 1) #TNC
lines(ALR.Residuals.Ord1$TaxiComp,col=Color.Scheme[2],type = "o",lty=2) #Taxi

###Residuals Histograms ###
par(mfrow=c(1,3))
hist(ALR.Residuals.Ord1$TNCComp,col = Color.Scheme[1])
hist(ALR.Residuals.Ord1$TaxiComp,col = Color.Scheme[2])
hist(ALR.Residuals.Ord1$CitiComp,col = Color.Scheme[3])

###Normal QQ Plots###
### Large Amounts of Skew in Data
par(mfrow=c(1,3))
qqnorm(ALR.Residuals.Ord1$TNCComp,col = Color.Scheme[1]);qqline(ALR.Residuals.Ord1$TNCComp)
qqnorm(ALR.Residuals.Ord1$TaxiComp,col = Color.Scheme[2]);qqline(ALR.Residuals.Ord1$TaxiComp)
qqnorm(ALR.Residuals.Ord1$CitiComp,col = Color.Scheme[3]);qqline(ALR.Residuals.Ord1$CitiComp)


### CCF for Residuals
windows()
acf(ALR.Residuals.Ord1)



