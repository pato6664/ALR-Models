### Exploratory Data Analysis-Redone so Plots are Consistent ###
### Five Number Summaries of Count ### 
FiveNum.Citi=ceiling(fivenum(Citi_Count))
FiveNum.Subway=fivenum(Subway_Count)
FiveNum.Taxi=fivenum(Taxi_Count)
FiveNum.TNC=trunc(fivenum(TNC_Count))
FiveNum.Total=ceiling(fivenum(NYC_Raw_Count_Total))
FiveNum.df=cbind.data.frame(FiveNum.Citi,FiveNum.Subway,
                            FiveNum.Taxi,FiveNum.TNC,
                            FiveNum.Total)
rownames(FiveNum.df)=c("Min.","Q1.","Median","Q3","Max.")
colnames(FiveNum.df)=c("Citi","Subway","Taxi","TNC","Total")
FiveNum.df


#### Count Plots ###
Color.Scheme=myColors <- c("red", "darkgreen", "darkviolet", "goldenrod", "darkblue")


#### Graphs of All Count Series-Includes Total ### 
windows()
par(mfrow=c(1,1))
plot.xts(Raw_Count.df$SubwayCt,xlab="Time",ylab="Count",main = "Modal Counts-All",
         major.ticks = 'months',ylim=c(0,9000000),minor.ticks = F,col = Color.Scheme[4],
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(x = Raw_Count.df$TNCCt, col = Color.Scheme[1])
lines(x = Raw_Count.df$TaxiCt, col = Color.Scheme[2])
lines(x = Raw_Count.df$CitiCt, col = Color.Scheme[3])
addLegend(legend.loc = 'topleft', legend.names = c("TNC","Taxi","Citi","Subway"), lty = 1, col = Color.Scheme[1:4])


### Graphs of TNC,Taxi, and Citi Counts ###
windows()
plot.xts(Raw_Count.df$TNCCt,xlab="Time",ylab="Count",main = "Modal Counts-TNC,Taxi,Citibike ",
         major.ticks = 'months',ylim=c(0,700000),
         minor.ticks = F,col = Color.Scheme[1],
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(x = Raw_Count.df$TaxiCt, col = Color.Scheme[2])
lines(x = Raw_Count.df$CitiCt, col = Color.Scheme[3])
addLegend(legend.loc = 'topleft', legend.names = c("TNC","Taxi","Citi"), lty = 1, col = Color.Scheme[1:3])

### Histograms of Modal Counts
par(mfrow=c(1,2))
hist(Raw_Count.df$TNCCt,main = "TNC Count",ylab = "",col = Color.Scheme[1]) # Right Skew 
hist(Raw_Count.df$TaxiCt,main = "Taxi Count",ylab = "",col = Color.Scheme[2]) # Approximately Normal 
hist(Raw_Count.df$CitiCt,main = "",ylab = "Citibike Count",col = Color.Scheme[3]) # Approximately Normal 
hist(Raw_Count.df$SubwayCt,main = "",ylab = "Subway Count",col = Color.Scheme[4]) ### Note bi-modality of Subway 

### Observation-When we create ALR components, Taxi becomes bi-modal, is this possibly due to issues of subway being bi-modal?

### Proportion Graphs- All 4 modes ### 
windows()
par(mfrow=c(1,1))
plot.xts(prop.df$TNCProp,xlab="Time",ylab="Proportion",main = "Modal Proportions (All 4 Modes)",
         major.ticks = 'years',ylim=c(0,1.2),minor.ticks = F,col = Color.Scheme[1],yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(x = prop.df$TaxiProp, col = Color.Scheme[2])
lines(x = prop.df$CitiProp, col = Color.Scheme[3])
lines(x = prop.df$SubwayProp, col = Color.Scheme[4])
addLegend(legend.loc = 'topleft',legend.names = c("TNC","Taxi","Citi","Subway"), lty = 1, col = Color.Scheme[1:4])

### Proportion Graphs- TNC,Taxi,and Citi ###
windows()

plot.xts(prop.df$TNCProp,xlab="Time",ylab="Proportion",main = "Modal Proportions (TNC,Taxi,Citi)",
         major.ticks = 'years',ylim=c(0,0.2),minor.ticks = F,col = Color.Scheme[1],
         yaxis.right = F,grid.ticks.on = T,grid.ticks.lty = 3)
lines(x = prop.df$TaxiProp, col = Color.Scheme[2])
lines(x = prop.df$CitiProp, col = Color.Scheme[3])
addLegend(legend.loc = 'topleft',legend.names = c("TNC","Taxi","Citi"), lty = 1, col = Color.Scheme[1:3])

### ACF for Proportions-TNC,Taxi,and Citi only ###
windows()
acf(prop.df[,1:3])


### ALR Components ###
windows()
par(mfrow=c(1,1))
plot.xts(component.df$TNCComp,xlab="Time",ylab="ALR Components",main = "ALR Components (Baseline = Subway)",
         major.ticks = 'years',minor.ticks = F,
         ylim=c(min(component.df),max(component.df)),
         col = Color.Scheme[1],grid.ticks.lty = 4)
lines(x = component.df$TaxiComp, col = Color.Scheme[2])
lines(x = component.df$CitiComp, col = Color.Scheme[3])
addLegend(legend.loc = 'bottomleft',legend.names = c("TNC","Taxi","Citi"), lty = 1, col = Color.Scheme[1:3])

### Histograms of ALR components
par(mfrow=c(1,3))
hist(component.df$TNCComp,main = "TNC Component",xlab = "",col = Color.Scheme[1])
hist(component.df$TaxiComp,main = "Taxi Component",xlab = "",col = Color.Scheme[2]) ### Note Bi-Modality of Taxi Component
hist(component.df$CitiComp,main = "Citibike Component",xlab = "",col = Color.Scheme[3])


### CCF of ALR Components ###
windows()
acf(component.df)
