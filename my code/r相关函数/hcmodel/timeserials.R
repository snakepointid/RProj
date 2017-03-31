library(ggplot2)
#plot analysis
timeserialsDF<-actionInfo[type==4&cate==8,]

timeserialsDF[,"time"]<-timeserialsDF$time%>%substr(.,1,10)%>%as.Date

timeDF<-timeserialsDF[,.(buyNum=.N),.(time)]
timeDF<-timeDF[order(time),]
write.csv(timeDF,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/timeserials.csv",row.names = F)
 
 