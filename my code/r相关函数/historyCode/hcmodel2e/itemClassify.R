 #time maping
actionInfo[,"day"]     <-actionInfo[,substr(time,1,10)]%>%as.character
actionInfo[,"hour"]    <-actionInfo[,substr(time,12,13)]%>%as.numeric
actionInfo[,"minute"]  <-actionInfo[,substr(time,15,16)]%>%as.numeric

earlday<-actionInfo$day%>%unique%>%as.Date%>%min-1
lastday<-as.Date("2016-04-15")
dateSpan<-seq(earlday,lastday,1)
dateSpan<-sort(dateSpan,decreasing = T)

vl<-dateSpan%>%length
windowP<-seq(1,vl,5)
windowdayMP             <-rep(dateSpan[windowP],each=5)[1:vl]
names(windowdayMP)      <-dateSpan%>%as.character 
actionInfo[,"windowday"]<-actionInfo[,windowdayMP[day]-4]
names(dateSpan)         <-dateSpan%>%as.character 
#overall statistics
 
#get training user id
postiveUser <- actionInfo[type==4&cate==8,]$user_id%>%unique
 #get train users' action info
positiveActionInfo<-actionInfo[user_id%in%postiveUser,]
predictActionInfo <-actionInfo[!user_id%in%postiveUser,]
#get train users' label info
posiUserTargday<-positiveActionInfo[type==4&cate==8,.(label=sku_id),by=.(user_id,windowday)]
posiUserTargday<-posiUserTargday[,.(targwindowday=windowday,user_id,label)]
predUserTargday<-predictActionInfo[,.(targwindowday=lastday+1,label=-1),.(user_id)]
#put together
UserTargdayLabel<-rbind(posiUserTargday,predUserTargday)
UserActionInfo<-rbind(positiveActionInfo,predictActionInfo)
UserActionInfo<-UserActionInfo%>%unique
UserTargdayLabel<-UserTargdayLabel%>%unique

#merge
UserActionInfo<-merge(UserActionInfo, UserTargdayLabel, by.x = 'user_id', by.y = 'user_id',allow.cartesian=T)
#clean mem
rm(UserTargdayLabel,actionInfo,posiUserTargday,predUserTargday,positiveActionInfo,predictActionInfo)
gc()
#time gap
UserActionInfo<-UserActionInfo[windowday<targwindowday,]
UserActionInfo[,"timegap"]<-(UserActionInfo$targwindowday-dateSpan[UserActionInfo$day])%>%as.numeric()*24*60-UserActionInfo$hour*60-UserActionInfo$minute
UserActionInfo<-UserActionInfo[timegap<30*24*60,]

#feature construct
timegapSeq<-c(5,10,30,60,2*60,4*60,24*60,3*24*60,7*24*60,30*24*60)
alldata<-findActMostItemBytime(UserActionInfo,timegapSeq) 

