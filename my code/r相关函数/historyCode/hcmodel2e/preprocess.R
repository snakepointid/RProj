#truncate the category
actionInfo[,"model_id"]<-ifelse(actionInfo$model_id%in%model_idMP,actionInfo$model_id,-1)
actionInfo[,"brand"]   <-ifelse(actionInfo$brand%in%brandMP,actionInfo$brand,-1)
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
# overallSTA<-getWindayInfoWraper(actionInfo)
# overallSTA[,'targwindowday']<-overallSTA$windowday+5
# overallSTA[,c('windowday'):=NULL]
# #get training user id
postiveUser <- actionInfo[type==4&cate==8,]$user_id%>%unique
negtiveUser <- sample(actionInfo$user_id%>%unique,3*length(postiveUser),replace = F)
negtiveUser <- negtiveUser[!negtiveUser%in%postiveUser]
#get train users' action info
positiveActionInfo<-actionInfo[user_id%in%postiveUser,]
negtiveActionInfo <-actionInfo[user_id%in%negtiveUser,]
predictActionInfo <-actionInfo[!user_id%in%postiveUser,]
#get train users' label info
BuyWindDay <-positiveActionInfo[type==4&cate==8,]$windowday 
posiUserTargday<-positiveActionInfo[type==4&cate==8,.(label=1),by=.(user_id,windowday)]
posiNegUserTargday<-rbind(posiUserTargday[,.(user_id=user_id,targwindowday=windowday-5 ,label=0)],
                          posiUserTargday[,.(user_id=user_id,targwindowday=windowday-10,label=0)],
                          posiUserTargday[,.(user_id=user_id,targwindowday=windowday-15,label=0)])
posiUserTargday<-posiUserTargday[,.(targwindowday=windowday,user_id,label)]
negUserTargday <-negtiveActionInfo[,.(targwindowday=c(max(windowdayMP)-4,sample(BuyWindDay,1,replace = F)),label=0),.(user_id)]
negUserTargday <-negUserTargday%>%unique
predUserTargday<-predictActionInfo[,.(targwindowday=lastday+1,label=-1),.(user_id)]
#put together
UserTargdayLabel<-rbind(posiUserTargday,posiNegUserTargday,negUserTargday,predUserTargday)
UserActionInfo<-rbind(positiveActionInfo,negtiveActionInfo,predictActionInfo)
UserActionInfo<-UserActionInfo%>%unique
UserTargdayLabel<-UserTargdayLabel%>%unique

#merge
UserActionInfo<-merge(UserActionInfo, UserTargdayLabel, by.x = 'user_id', by.y = 'user_id',allow.cartesian=T)
#clean mem
rm(UserTargdayLabel,posiNegUserTargday,actionInfo,posiUserTargday,negUserTargday,predUserTargday,positiveActionInfo,negtiveActionInfo,predictActionInfo)
gc()
#time gap
UserActionInfo<-UserActionInfo[windowday<targwindowday,]
UserActionInfo[,"timegap"]<-(UserActionInfo$targwindowday-dateSpan[UserActionInfo$day])%>%as.numeric()*24*60-UserActionInfo$hour*60-UserActionInfo$minute
 
source('~/Documents/project/JDproj/hcmodel2e/featureConstruct.R')
 
