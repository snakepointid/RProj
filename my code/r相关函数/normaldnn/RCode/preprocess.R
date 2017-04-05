#truncate the category
type_ALL<-actionInfo$type%>%unique
cate_ALL<-actionInfo$cate%>%unique

actionInfo[model_id%>%is.na,"model_id"]<--1
model_id_ALL<- actionInfo$model_id%>%unique
brand_ALL<-c(-1,actionInfo[cate==8,]$brand%>%unique)%>%unique
actionInfo[,"brand"]   <-ifelse(actionInfo$brand%in%brand_ALL,actionInfo$brand,-1)
all8item<-actionInfo[cate==8,]$sku_id%>%unique
actionInfo[!sku_id%in%all8item,'sku_id']<--1
buyed8item<-actionInfo[cate==8&type==4,]$sku_id%>%unique
popItem<-actionInfo[cate==8&type==4,]$sku_id%>%table
popItem<-popItem[popItem>30]%>%names%>%as.integer
all8item<-all8item[!all8item%in%buyed8item]
buyed8item<-buyed8item[!buyed8item%in%popItem]
actionInfo[sku_id%in%all8item,'sku_id']<--2
actionInfo[sku_id%in%buyed8item,'sku_id']<--3
items_ALL<-c(-1,-2,-3,popItem)%>%unique
#time maping
actionInfo[,"day"]     <-actionInfo[,substr(time,1,10)]%>%as.character
actionInfo[,"hour"]    <-actionInfo[,substr(time,12,13)]%>%as.numeric
actionInfo[,"minute"]  <-actionInfo[,substr(time,15,16)]%>%as.numeric
 
earlday<-actionInfo$day%>%unique%>%as.Date%>%min-4
lastday<-as.Date("2016-04-15")
dateSpan<-seq(earlday,lastday,1)
dateSpan<-sort(dateSpan,decreasing = T)

vl<-dateSpan%>%length
windowP<-seq(5,vl+5,5)
windowdayMP             <-rep(dateSpan[windowP],each=5)[1:vl]
names(windowdayMP)      <-dateSpan%>%as.character 
actionInfo[,"windowday"]<-actionInfo[,windowdayMP[day]]
names(dateSpan)         <-dateSpan%>%as.character 
#get positive and negtive samples
# postiveUser <- actionInfo[type==4&cate==8,]$user_id%>%unique
# negtiveUser <- sample(actionInfo$user_id%>%unique,2*length(postiveUser),replace = F)
# negtiveUser <- negtiveUser[!negtiveUser%in%postiveUser]
# #get train users' action info
# positiveActionInfo<-actionInfo[user_id%in%postiveUser,]
# negtiveActionInfo <-actionInfo[user_id%in%negtiveUser,]
# predictActionInfo <-actionInfo[!user_id%in%postiveUser,]
# #get train users' label info
# BuyWindDay <-positiveActionInfo[type==4&cate==8,]$windowday 
# posiUserTargday<-positiveActionInfo[type==4&cate==8,.(label=1),by=.(user_id,windowday)]
# posiNegUserTargday<-rbind(posiUserTargday[,.(user_id=user_id,targwindowday=windowday-5 ,label=0)],
#                           posiUserTargday[,.(user_id=user_id,targwindowday=windowday-10,label=0)],
#                           posiUserTargday[,.(user_id=user_id,targwindowday=windowday-15,label=0)])
# posiUserTargday<-posiUserTargday[,.(targwindowday=windowday,user_id,label)]
# negUserTargday <-negtiveActionInfo[,.(targwindowday=c(max(windowdayMP)-4,sample(BuyWindDay,1,replace = F)),label=0),.(user_id)]
# negUserTargday <-negUserTargday%>%unique
# predUserTargday<-predictActionInfo[,.(targwindowday=lastday+1,label=-1),.(user_id)]
# #put together
# UserTargdayLabel<-rbind(posiUserTargday,posiNegUserTargday,negUserTargday,predUserTargday)
# #merge
# UserActionInfo<-merge(UserActionInfo, UserTargdayLabel, by.x = 'user_id', by.y = 'user_id',allow.cartesian=T)
# #clean mem
# rm(posiUserTargday,posiNegUserTargday,negUserTargday,predUserTargday)
# gc()
# #time gap
# UserActionInfo<-UserActionInfo[windowday<targwindowday,]
# UserActionInfo[,"timegap"]<-(UserActionInfo$targwindowday-dateSpan[UserActionInfo$day])%>%as.numeric()*24*60-UserActionInfo$hour*60-UserActionInfo$minute
#  
 source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/rÏà¹Øº¯Êý/normaldnn/RCode/featureConstruct.R')
#  
