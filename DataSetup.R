rm(list = ls())

library(xts) ### All relevant series are turned into xts objects
library(compositions) ### Used to create ALR compositions-more simple than using bxcx for 'FitAR' package

#### Data Setup ### 
modal_nyc_trips = read.csv("D:\\Time Series\\RideShare Models\\Data\\agg_modal_nyc_trips_daily_jan15-jun17_imp_green.csv")
nyc_weather = read.csv("D:\\Time Series\\RideShare Models\\Data\\all_nyc_weather.csv")
nyc_events = read.csv("D:\\Time Series\\RideShare Models\\Data\\all_nyc_events_raw15-17.csv")
nyc_holidays = read.csv("D:\\Time Series\\RideShare Models\\Data\\holidays-nyc.csv")
nyc_peak = read.csv("D:\\Time Series\\RideShare Models\\Data\\nyc_peak_travel_season.csv")

### Create a date sequence starting on April 1st,2015, ending June 30th, 2017
ts.length=length(modal_nyc_trips$count_subway) ### length of original series
april.1st=91 # Denotes start date for analysis - first day we had all TNC providers in data set.
subset.length=length(modal_nyc_trips$count_total[april.1st:ts.length]) ### Value should be 822-total obs for subsetted data
Full.Dates=seq(as.Date("2015-04-01"),length.out = length(modal_nyc_trips$count_total[april.1st:ts.length]),by="days")


#### Modal Counts ###
TNC_Count=rowSums(modal_nyc_trips[april.1st:ts.length,3:5])
Taxi_Count=rowSums(modal_nyc_trips[april.1st:ts.length,6:7])
Citi_Count=modal_nyc_trips$count_citi[april.1st:ts.length]
Subway_Count=modal_nyc_trips$count_subway[april.1st:ts.length]

### Looking for zeros in endogenous time series ### 
TNC_zero_ind=as.vector(which(TNC_Count %in% c(0,NA)))
Taxi_zero_ind=as.vector(which(Taxi_Count %in% c(0,NA)))
Citi_zero_ind=as.vector(which(Citi_Count %in% c(0,NA))) ### Citi Bike has 8 days where count=0, impute via interpolation? ### 
sub_zero_ind=as.vector(which(Subway_Count %in% c(0,NA)))  
################################################################################################################################
TNC_zero_ind
Taxi_zero_ind
Citi_zero_ind
sub_zero_ind

Citi_Count=replace(Citi_Count,Citi_zero_ind,values = mean(Citi_Count)) ### replace zero counts by NA ###
Citi_zero_ind=as.vector(which(Citi_Count %in% c(0,NA))) ### Citi Bike has 8 days where count=0-impute via global mean of citibike count ### 
Citi_zero_ind

NYC_Raw_Count_Total=TNC_Count+Taxi_Count+Citi_Count+Subway_Count
Raw_Count.df=xts(cbind.data.frame(TNC_Count,Taxi_Count,Citi_Count,Subway_Count,NYC_Raw_Count_Total),order.by = Full.Dates,by="days")
colnames(Raw_Count.df)=c("TNCCt","TaxiCt","CitiCt","SubwayCt","TotRawCt")

### Constructing Proportions- X_ti=C_ti/C_t,total ### 
X_t1=TNC_Count/NYC_Raw_Count_Total
X_t2=Taxi_Count/NYC_Raw_Count_Total
X_t3=Citi_Count/NYC_Raw_Count_Total
X_t4=Subway_Count/NYC_Raw_Count_Total ### X_tg-use subway as baseline for Model ### 
prop.df=xts(cbind.data.frame(X_t1,X_t2,X_t3,X_t4),order.by = Full.Dates,by='days')
colnames(prop.df)=c("TNCProp","TaxiProp","CitiProp","SubwayProp")


### Use alr() function from compositions library to create ALR components 
component.df=xts(alr(prop.df),order.by = Full.Dates,by='days');#View(component.df) # Uses last column of prop.df as baseline-so subway is now the baseline component
colnames(component.df)=c("TNCComp","TaxiComp","CitiComp")


#### Create a data frame for weekly seasonal indicators ###
wed=rep(c(1,0,0,0,0,0,0),length=subset.length)
thu=rep(c(0,1,0,0,0,0,0),length=subset.length)
fri=rep(c(0,0,1,0,0,0,0),length=subset.length)
sat=rep(c(0,0,0,1,0,0,0),length=subset.length)
sun=rep(c(0,0,0,0,1,0,0),length=subset.length)
mon=rep(c(0,0,0,0,0,1,0),length=subset.length)
tue=rep(c(0,0,0,0,0,0,1),length=subset.length)
weekly.df=xts(cbind.data.frame(wed,thu,fri,sat,sun,mon,tue),order.by = Full.Dates,by='days') 
colnames(weekly.df)=c("Wed","Thu","Fri","Sat","Sun","Mon","Tue")

### Create a data frame of exogenous predictors-
exog.df=xts(cbind.data.frame(nyc_weather$PRCP_NYC[april.1st:ts.length],
                             "event_count"=nyc_events$event_count[april.1st:ts.length],
                             nyc_holidays$major_holidays[april.1st:ts.length],
                             nyc_peak$holiday_season[april.1st:ts.length],
                             Citi_Peak=nyc_peak$citi_peak[april.1st:ts.length]),
            order.by = Full.Dates,by='days')
colnames(exog.df)=c("Precip","Event","Holidays","PeakTrav","Citi_Peak")

### Subset into a training and holdout set-holdout set will be 100 days long (A more round number now) ###
train_len=737
### Training Set
Train.Df=cbind.data.frame(component.df[1:train_len,1:3],weekly.df[1:train_len],exog.df[1:train_len])
str(Train.Df)


### Holdout Set 
Exog.Hold.df=cbind.data.frame(weekly.df[(train_len+1):subset.length],exog.df[(train_len+1):subset.length]);str(Exog.Hold.df)
Hold.DF=cbind.data.frame(component.df[(train_len+1):subset.length,1:3],weekly.df[(train_len+1):subset.length],exog.df[(train_len+1):subset.length])
str(Hold.DF)
