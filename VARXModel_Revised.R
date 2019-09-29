# Attempt to build a model that is clean temporally 
# create only six indicators to allow for an intercept to be fit 

# Only way it seems to fix this is to have p=7-way overfit it seems though

ALR.Revised=VAR(Train.Df[,1:3],p=7,exogen=Train.Df[,4:15],type = 'both')
summary(ALR.Revised)
ALR.Revised.Residuals=resid(ALR.Revised)
acf(ALR.Revised.Residuals)
