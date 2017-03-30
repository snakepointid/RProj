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
allActionInfo<-rbind(act02,act03,act03_extra,act04)
rm(act02,act03,act03_extra,act04)
gc()
#sample data
samp<-sample(c(1:(allActionInfo$user_id%>%length)),10000000,F)
sampPosi<-which(allActionInfo$cate==8&allActionInfo$type==4)
samp<-c(samp,sampPosi)%>%unique
actionInfo<-allActionInfo[samp,]
#brand and modek_id top
model_idMP<-(actionInfo[cate==8,]$model_id%>%table%>%sort(.,T)%>%names)[1:5]%>%as.integer%>%sort
brandMP<-(actionInfo[cate==8,]$brand%>%table%>%sort(.,T)%>%names)[1:5]%>%as.integer%>%sort
actionInfo[model_id%>%is.na,"model_id"]<--2
model_idMP<-c(-2,-1,model_idMP)
brandMP<-c(-1,brandMP)
source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/Rcode/basicFunc.R')
actionInfo[,"model_id"]<-ifelse(actionInfo$model_id%in%model_idMP,actionInfo$model_id,-1)
actionInfo[,"brand"]<-ifelse(actionInfo$brand%in%brandMP,actionInfo$brand,-1)
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
 
#find strong rules
postiveUser<-actionInfo[type==4&cate==8,]$user_id%>%unique
negtiveUser<- sample(actionInfo$user_id%>%unique,length(postiveUser),replace = F)
negtiveUser<-negtiveUser[!negtiveUser%in%postiveUser]
ruleRejctUser<-actionInfo[cate!=8,]$user_id%>%unique
ruleRejctUser<-c(ruleRejctUser,postiveUser)
#truncate
positiveDF<-actionInfo[user_id%in%postiveUser,]
negtiveDF <-actionInfo[user_id%in%c(negtiveUser),]
predictDF <-actionInfo[!user_id%in%ruleRejctUser,]

posiWind<-positiveDF[type==4&cate==8,.(label=1),by=.(user_id,windowday)]
prenegWind<-posiWind[,.(user_id=user_id,targwindowday=windowday-5)]
negwind<-negtiveDF[,.(targwindowday=max(windowday)),.(user_id)]
posiWind<-posiWind[,.(user_id=user_id,targwindowday=windowday)]
negwind<-rbind(negwind,prenegWind)
negtiveDF<-rbind(negtiveDF,positiveDF)
#
setkey(posiWind,user_id)
setkey(negwind,user_id)
setkey(positiveDF,user_id)
setkey(negtiveDF,user_id)

predictDF[,'targwindowday']<-lastday+1
positiveDF<-posiWind[positiveDF]
positiveDF<-positiveDF[windowday<targwindowday,]
negtiveDF<-negwind[negtiveDF]
negtiveDF<-negtiveDF[windowday<targwindowday,]
#time gap
negtiveDF[,"timegap"]<-(negtiveDF$targwindowday-dateSpan[negtiveDF$day])%>%as.numeric()*24*60-negtiveDF$hour*60-negtiveDF$minute
predictDF[,"timegap"]<-(predictDF$targwindowday-dateSpan[predictDF$day])%>%as.numeric()*24*60-predictDF$hour*60-predictDF$minute
positiveDF[,"timegap"]<-(positiveDF$targwindowday-dateSpan[positiveDF$day])%>%as.numeric()*24*60-positiveDF$hour*60-positiveDF$minute
#
featname<-c(paste("model_id",c(model_idMP),"times",sep="_"),paste("model_id",c(model_idMP),"prop",sep="_"),
            paste("type",c(1:6),"times",sep="_"),paste("type",c(1:6),"prop",sep="_"),
            paste("brand",c(brandMP),"times",sep="_"),paste("brand",c(brandMP),"prop",sep="_"))
timegapSeq<-c(1,5,10,30,60,4*60,24*60,3*24*60,7*24*60,30*24*60)
positiveDF<-getFeat(positiveDF,timegapSeq,featname) 
negtiveDF<-getFeat(negtiveDF,timegapSeq,featname) 
predictDF<-getFeat(predictDF,timegapSeq,featname)
positiveDF[,"label"]<-1
negtiveDF[,"label"]<-0
alldata<-rbind(positiveDF,negtiveDF)
#miss imp
alldata<-data.frame(alldata,stringsAsFactors = F)
predictDF<-data.frame(predictDF,stringsAsFactors = F)
for(i in names(alldata)){
  if(i=="user_id")next
  p<-which(alldata[,i]%>%is.na|alldata[,i]%>%is.nan|alldata[,i]=="NA")
 
  alldata[p,i]<-0
  alldata[,i]<-alldata[,i]%>%as.numeric
}
for(i in names(predictDF)){
  if(i=="user_id")next
  p<-which(predictDF[,i]%>%is.na|predictDF[,i]%>%is.nan|predictDF[,i]=="NA")
  predictDF[p,i]<-0
  predictDF[,i]<-predictDF[,i]%>%as.numeric
}

source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/hcmodel/constructModel.R')