library(data.table)
library(dplyr)
#read date
source('~/Documents/project/RProj/my code/jdProj/code2/basicFunc.R')
maxrows=10000000
act02<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Action_201602.csv',nrows = maxrows)
act03<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Action_201603/JData_Action_201603.csv',nrows = maxrows)
act03_extra<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Action_201603/JData_Action_201603_extra.csv',nrows = maxrows)
act04<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Action_201604.csv',nrows = maxrows)
userInfo<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_User.csv',encoding="UTF-8")
productInfo<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Product.csv')
commentInfo<-fread('/Users/snakepointid/Documents/project/RProj/my code/jdProj/rawdata/JData_Comment(修正版).csv')
#merge and delete
actionInfo<-rbind(act02,act03,act03_extra,act04)
rm(act02,act03,act03_extra,act04)
gc() 
#maping
buytypeMP<-c("browse","buycar","cancel","order","star","hit")
actionInfo[,'type']<-actionInfo[,buytypeMP[type]]
#time map
actionInfo[,"day"]<-actionInfo[,substr(time,1,10)]
actionInfo[,"hour"]<-actionInfo[,substr(time,12,13)]%>%as.numeric
actionInfo[,"minute"]<-actionInfo[,substr(time,15,16)]%>%as.numeric
date<-actionInfo$day%>%unique%>%as.Date
earlday<-min(date)-1
lastday<-as.Date("2016-04-15")
date<-seq(earlday,lastday,1)
cutday<-sort(date,decreasing = T)
weekday<-cutday%>%weekdays
names(weekday)<-cutday
actionInfo[,"weekday"]<-actionInfo[,weekday[day]]
vl<-cutday%>%length
cutp<-seq(1,vl,5)
cutdayMP<-rep(cutday[cutp],each=5)[1:vl]
names(cutdayMP)<-cutday 
actionInfo[,"windowday"]<-actionInfo[,cutdayMP[day]]
actionInfo[,"day"]<-actionInfo[,cutdayMP[day]]
#whether buy
actionInfo[,'historyBuyed']<-actionInfo[,as.numeric(type=="order"&cate==8)]
actionInfo<-actionInfo[order(user_id,windowday),]
actionInfo[,'historyBuyed']<-actionInfo[,as.numeric(cumsum(historyBuyed)>0),.(user_id)][,'V1']
#get label
targInfo<-actionInfo[,.(label=getlabel(sku_id,type,cate)),by=.(user_id,windowday)]
targInfo<-targInfo[label!=-2,]
targInfo[,"windowday"]<-targInfo[,windowday-5]
#get action
setkey(actionInfo,user_id,windowday)
setkey(targInfo,user_id,windowday)
actionInfo<-merge(actionInfo,targInfo)
 
actionInfo<-actionInfo[order(user_id,windowday,sku_id,type,-day,-hour,-minute),]
actionInfo<-actionInfo[,.(cate=first(cate),actimes=round(1/.N,3),brand=first(brand),day=first(day),hour=first(hour),minute=first(minute)),
           .(user_id,windowday,historyBuyed,sku_id,type)]
actionInfo[,'timegap']<-actionInfo[,ifelse(day==windowday,round(1/(60*(24-hour)-minute),3),0)]
actionInfo<-actionInfo[order(user_id,windowday,day,hour,minute),]
actionInfo[,'act']<-actionInfo[,paste(type,cate,brand,sku_id,actimes,timegap,historyBuyed,sep="#")]
actionInfo<-actionInfo[,.(action=paste(act,collapse = "->")),.(user_id,windowday)]
#merge
setkey(actionInfo,user_id,windowday)
setkey(targInfo,user_id,windowday)
finalDF<-actionInfo[targInfo]
 