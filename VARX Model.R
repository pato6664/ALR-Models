### VARX Modeling ### 
library(vars) ### vars package used for ALR model fitting 

### Returns a list of VAR(p) models where a VAR(p)-p is specified by user in function call ###
"VARX.Est"=function(endog.series,order){
  varx_list=list()
  
  for(i in 1:order)
  {
    var_fit=VAR(endog.series, p=0+i,season =NULL,exogen = Train.Df[,4:15] ,type="trend")
    varx_list[[i]]=var_fit
    names(varx_list)[i]=paste("Fit for",i)
    
  }
  return(varx_list)  
}

ALR.Model.List=VARX.Est(endog.series=Train.Df[,1:3],order = 10)

### Automated VARSelect Function-returns results of varselect for each box cox transform ###
### In-Sample Selection Based on BIC ###
#VARselect()

BIC.ALR=c()
for(i in 1:length(ALR.Model.List)){
  
  BIC.ALR[i]=BIC(ALR.Model.List[[i]])
}
plot(1:length(BIC.ALR),BIC.ALR,main="BIC-ALR VARX(p)",xlab="Order of Auto-Regression",ylab="BIC",type="o",lty=1)

### Finding Maximum and Minimum BIC ### 
min.bic=which(BIC.ALR==min(BIC.ALR)); min.bic
max.bic=which(BIC.ALR==max(BIC.ALR));max.bic

summary(ALR.Model.List$`Fit for 1`)


