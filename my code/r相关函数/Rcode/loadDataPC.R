library(data.table)
library(dplyr)
#read date
source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/Rcode/basicFunc.R')
maxrows=200000
act02<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201602.csv',nrows = maxrows)
act03<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201603.csv',nrows = maxrows)
act03_extra<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201603_extra.csv',nrows = maxrows)
act04<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Action_201604.csv',nrows = maxrows)
productInfo<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/rawdata/JData_Product.csv')
#merge and delete
actionInfo<-rbind(act02,act03,act03_extra,act04)
rm(act02,act03,act03_extra,act04)
gc() 
#maping
buytypeMP<-c("browse","buycar","cancel","order","star","hit")
actionInfo[,'type']<-actionInfo[,buytypeMP[type]]
#indexmap and save
buyedInfo<-actionInfo[type=="order",]
 
prodEmbIndx<-buyedInfo$sku_id%>%table%>%sort(.,T)
prodEmbIndxMP<-data.table(mp=prodEmbIndx%>%names%>%as.integer,index=c(1:length(prodEmbIndx)))

userEmbIndx<-buyedInfo$user_id%>%table%>%sort(.,T)
userEmbIndxMP<-data.table(mp=userEmbIndx%>%names%>%as.integer,index=c(1:length(userEmbIndx)))

brandEmbIndx<-buyedInfo$brand%>%table%>%sort(.,T)
brandEmbIndxMP<-data.table(mp=brandEmbIndx%>%names%>%as.integer,index=c(1:length(brandEmbIndx)))

targProdEmbIndx<-buyedInfo[cate==8,]$sku_id%>%table%>%sort(.,T)
targProdEmbIndxMP<-data.table(mp=targProdEmbIndx%>%names%>%as.integer,index=c(1:length(targProdEmbIndx)))
write.csv(prodEmbIndxMP,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/prodEmbIndxMP.csv",row.names = F)
write.csv(userEmbIndxMP,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/userEmbIndxMP.csv",row.names = F)
write.csv(brandEmbIndxMP,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/brandEmbIndxMP.csv",row.names = F)
write.csv(targProdEmbIndxMP,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/targProdEmbIndxMP.csv",row.names = F)

#time map
actionInfo[,"day"]<-actionInfo[,substr(time,1,10)]%>%as.character
actionInfo[,"hour"]<-actionInfo[,substr(time,12,13)]%>%as.numeric
actionInfo[,"minute"]<-actionInfo[,substr(time,15,16)]%>%as.numeric

earlday<-actionInfo$day%>%unique%>%as.Date%>%min-1
lastday<-as.Date("2016-04-15")
dateSpan<-seq(earlday,lastday,1)
dateSpan<-sort(dateSpan,decreasing = T)

weekMP<-c(0:6)
names(weekMP)<-c( "星期一" ,"星期二","星期三","星期四","星期五","星期六", "星期日" )
weekday<-weekMP[dateSpan%>%weekdays]
names(weekday)<-dateSpan%>%as.character 
actionInfo[,"weekday"]<-actionInfo[,weekday[day]]

vl<-dateSpan%>%length
windowP<-seq(1,vl,5)
windowdayMP<-rep(dateSpan[windowP],each=5)[1:vl]
names(windowdayMP)<-dateSpan%>%as.character 
actionInfo[,"windowday"]<-actionInfo[,windowdayMP[day]]
 
names(dateSpan)<-dateSpan%>%as.character 
actionInfo[,"day"]<-actionInfo[,dateSpan[day]]
#whether buy
actionInfo[,'historyBuyed']<-actionInfo[,as.numeric(type=="order"&cate==8)]
actionInfo<-actionInfo[order(user_id,windowday),]
actionInfo[,'historyBuyed']<-actionInfo[,as.numeric(cumsum(historyBuyed)>0),.(user_id)]$V1
#get label
targInfo<-actionInfo[,.(label=getlabel(sku_id,type,cate)),by=.(user_id,windowday)]
targInfo<-targInfo[label!=-2,]
targInfo[,"windowday"]<-targInfo[,windowday-5]
addtarg<-targInfo[,.(user_id)]%>%unique
addtarg[,"windowday"]<-lastday
addtarg[,"label"]<--2
targInfo<-rbind(targInfo,addtarg)
#get action
max_seq_length<-30
setkey(actionInfo,user_id,windowday)
setkey(targInfo,user_id,windowday)
actionInfo<-merge(actionInfo,targInfo)

actionInfo<-actionInfo[order(user_id,windowday,sku_id,type,-day,-hour,-minute),]
actionInfo<-actionInfo[,.(cate=first(cate),actimes=round(1/.N,3),brand=first(brand),weekday=first(weekday),day=first(day),hour=first(hour),minute=first(minute)),
                       .(user_id,windowday,historyBuyed,sku_id,type)]
actionInfo[,'timegap']<-actionInfo[,ifelse(day==windowday,round(1/(60*(24-hour)-minute),3),0)]
actionInfo<-actionInfo[order(user_id,windowday,-day,-hour,-minute),]
actionInfo[,'act']<-actionInfo[,paste(user_id,weekday,type,cate,brand,sku_id,actimes,timegap,historyBuyed,sep="#")]
actionInfo<-actionInfo[,.(action=paste(act[1:min(length(act),max_seq_length)],collapse = "<-")),.(user_id,windowday)]
#merge and save
setkey(actionInfo,user_id,windowday)
setkey(targInfo,user_id,windowday)
finalDF<-actionInfo[targInfo]
finalDF<-finalDF[!action%>%is.na,]
write.csv(finalDF,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/action0328.csv",row.names = F)
