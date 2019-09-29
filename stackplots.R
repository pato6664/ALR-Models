#### Script is for Saving Stack Plots to PDF
### Stack Plots of Forecasts for:Total Count,TNC Count,and TNC Proportion
TNC.Stack1=plot.xts(TNC.Count.Pred,main = "TNC Forecast Count",
                    col='blue',type = 'o',pch=16,lty=1,ylim=c(min(Raw_Count.df$TNCCt[738:822]),max(TNC.Count.Pred)),
                    grid.col = NA,yaxis.right = F)
TNC.Stack1=lines(Raw_Count.df$TNCCt[738:822],col = 'black',type = 'l',lty = 2)
TNC.Stack1=addLegend(legend.loc = 'topright',legend.names = c("Forecast","Observed"),
                     col = c('blue','black'),lty=c(1,2),pch=c(16,NA))
windows()
TNC.Stack1


### Total Count
TNC.Stack2=plot.xts(as.xts(Total.Predict$fit),main="Total Count Forecast",
                    col='red',type = 'o',pch=16,lty=1,
                    yaxis.right = F,grid.col = NA,
                    ylim = c(min(Raw_Count.df$TotRawCt[738:822]-500000),max(Raw_Count.df$TotRawCt[738:822]+500000)))
TNC.Stack2=lines(Raw_Count.df$TotRawCt[738:822],col = 'black',type = 'l',lty = 2)
TNC.Stack2=addLegend(legend.loc = 'topright',legend.names = c("Forecast","Observed"),
                     col = c('red','black'),lty=c(1,2),pch=c(16,NA))

windows()
TNC.Stack2

### TNC Proportions
TNC.Stack3=plot.xts(Prop.PointFcst.df[,1],main = "TNC Forecast Proportion",
                    col='purple',type = 'o',pch=16,lty=1,ylim=c(0.04,.16),yaxis.right  = F,grid.col = NA)
TNC.Stack3=lines(Raw_Count.df$TNCCt[738:822]/Raw_Count.df$TotRawCt[738:822],col = 'black',type = 'l',lty = 2)
TNC.Stack3=addLegend(legend.loc = 'topright',legend.names = c("Forecast","Observed"),
                     col = c('purple','black'),lty=c(1,2),pch=c(16,NA))

windows()
TNC.Stack3

#windows()
pdf("TNCStackPlot1.pdf",width=7,height = 3,paper = 'special')
TNC.Stack1
dev.off()

pdf("TNCStackPlot2.pdf",width=7,height = 3,paper = 'special')
TNC.Stack2
dev.off()

pdf("TNCStackPlot3.pdf",width=7,height = 3,paper = 'special')
TNC.Stack3
dev.off()

