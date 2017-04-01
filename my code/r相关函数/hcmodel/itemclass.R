library(data.table)
library(dplyr)
#read date

maxrows=-1
act02<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201602.csv',nrows = maxrows)
act03<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201603.csv',nrows = maxrows)
act03_extra<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201603_extra.csv',nrows = maxrows)
act04<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201604.csv',nrows = maxrows)
productInfo<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Product.csv')
#merge and delete
actionInfo<-rbind(act02,act03,act03_extra,act04)
rm(act02,act03,act03_extra,act04)
gc()
#
 
#truncate
actionInfo<-actionInfo[cate==8,]
buyedUser<-actionInfo[type==4,]$user_id%>%unique
actionInfo<-actionInfo[user_id%in%buyedUser,]
#GET POP ITEM
skuCount<-actionInfo[type==4&cate==8,]$sku_id%>%table
popItem<-skuCount[skuCount>50]%>%names%>%as.integer
actionInfo[,"popitem"]<-ifelse(actionInfo$sku_id%in%popItem,actionInfo$sku_id,-1)
#get user item pair
user_item_pair<-actionInfo[type==4&cate==8,.(user_id,popitem)]
 
#time map 
#maping
actionInfo[,"day"]<-actionInfo[,substr(time,1,10)]%>%as.character
actionInfo[,"hour"]<-actionInfo[,substr(time,12,13)]%>%as.numeric
actionInfo[,"minute"]<-actionInfo[,substr(time,15,16)]%>%as.numeric

earlday<-actionInfo$day%>%unique%>%as.Date%>%min-1
lastday<-as.Date("2016-04-15")
dateSpan<-seq(earlday,lastday,1)
dateSpan<-sort(dateSpan,decreasing = T)

vl<-dateSpan%>%length
windowP<-seq(1,vl,5)
windowdayMP<-rep(dateSpan[windowP],each=5)[1:vl]
names(windowdayMP)<-dateSpan%>%as.character 
actionInfo[,"windowday"]<-actionInfo[,windowdayMP[day]-4]

names(dateSpan)<-dateSpan%>%as.character 
 
#truncate
positiveDF<-actionInfo 
actionInfo$user_id%>%unique%>%length
posiWind<-positiveDF[type==4&cate==8,.(label=1),by=.(user_id,windowday)]
 
posiWind<-posiWind[,.(user_id=user_id,targwindowday=windowday)]
 
#
setkey(posiWind,user_id)
setkey(positiveDF,user_id)
positiveDF<-posiWind[positiveDF]
positiveDF$user_id%>%unique%>%length
positiveDF<-positiveDF[windowday<targwindowday,]
 
#time gap
positiveDF[,"timegap"]<-(positiveDF$targwindowday-dateSpan[positiveDF$day])%>%as.numeric()*24*60-positiveDF$hour*60-positiveDF$minute
actInfo<-findActMostItemBytime(positiveDF,c(5,30,60,2*60,4*60,24*60,3*24*60,7*24*60))
#put together
setkey(actInfo,user_id)
setkey(user_item_pair,user_id)
alldata<-actInfo[user_item_pair]
