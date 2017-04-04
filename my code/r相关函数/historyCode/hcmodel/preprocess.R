#user info one hot 
ageMP<-userInfo$age%>%unique%>%oneHotEncode
userInfo[,paste("age",c(1:length(ageMP)),sep="_")]<-do.call(rbind,ageMP[userInfo$age%>%as.character]%>%strsplit(.,"#"))%>%data.table
sexMP<-userInfo$sex%>%unique%>%oneHotEncode
userInfo[,paste("sex",sexMP%>%names,sep="_")]<-do.call(rbind,sexMP[userInfo$sex%>%as.character]%>%strsplit(.,"#"))%>%data.table
user_lv_cdMP<-userInfo$user_lv_cd%>%unique%>%oneHotEncode
userInfo[,paste("user_lv_cd",user_lv_cdMP%>%names,sep="_")]<-do.call(rbind,user_lv_cdMP[userInfo$user_lv_cd%>%as.character]%>%strsplit(.,"#"))%>%data.table
userInfo[,"user_reg_dt"]<-(as.Date("2017-03-30")-userInfo$user_reg_dt%>%as.Date())%>%as.numeric
userInfo[,c('age',"sex","user_lv_cd"):=NULL]
 
#brand and modek_id top
maxCateNum<-6
model_id8MP<-(actionInfo[cate==8,]$model_id%>%table%>%sort(.,T)%>%names)[1:maxCateNum]%>%as.integer%>%sort
model_idMP<-(actionInfo$model_id%>%table%>%sort(.,T)%>%names)[1:maxCateNum]%>%as.integer%>%sort
brand8MP  <-(actionInfo[cate==8,]$brand%>%table%>%sort(.,T)%>%names)[1:maxCateNum]%>%as.integer%>%sort
brandMP   <-(actionInfo$brand%>%table%>%sort(.,T)%>%names)[1:maxCateNum]%>%as.integer%>%sort

model_idMP<-c(-2,-1,model_idMP,model_id8MP)%>%unique
brandMP  <-c(-1,brand8MP,brandMP)%>%unique
#load functions
source('/Users/snakepointid/Documents/project/JDproj/hcmodel/basicFunc.R')
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
overallSTA<-getWindayInfoWraper(actionInfo)
overallSTA[,'targwindowday']<-overallSTA$windowday+5
overallSTA[,c('windowday'):=NULL]
#get training user id
postiveUser <- actionInfo[type==4&cate==8,]$user_id%>%unique
negtiveUser <- sample(actionInfo$user_id%>%unique,2*length(postiveUser),replace = F)
negtiveUser <- negtiveUser[!negtiveUser%in%postiveUser]
ruleRejctUser<-c(postiveUser)
#truncate

positiveDF<-actionInfo[user_id%in%postiveUser,]
negtiveDF <-actionInfo[user_id%in%c(negtiveUser),]
predictDF <-actionInfo[!user_id%in%ruleRejctUser,]
#clean mem
# rm(actionInfo)
gc()
#
posiWind   <-positiveDF[type==4&cate==8,.(label=1),by=.(user_id,windowday)]
prenegWind <-posiWind[,.(user_id=user_id,targwindowday=windowday-5)]
negwind    <-negtiveDF[,.(targwindowday=max(windowday)),.(user_id)]
posiWind   <-posiWind[,.(user_id=user_id,targwindowday=windowday)]
negwind    <-rbind(negwind,prenegWind)
negtiveDF  <-rbind(negtiveDF,positiveDF)
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
featname<-c(paste("model_id8",c(model_idMP),"times",sep="_"),paste("model_idall",c(model_idMP),"times",sep="_"),paste("model_id8",c(model_idMP),"prop",sep="_"),
            paste("type8",c(1:6),"times",sep="_"),paste("typeall",c(1:6),"times",sep="_"),paste("type8",c(1:6),"prop",sep="_"),
            paste("brand8",c(brandMP),"times",sep="_"),paste("brandall",c(brandMP),"times",sep="_"),paste("brand8",c(brandMP),"prop",sep="_") )
timegapSeq<-c(1,5,10,30,60,2*60,4*60,24*60,3*24*60,7*24*60,30*24*60)
positiveDF<-windowFeature(positiveDF,timegapSeq,featname) 
negtiveDF<-windowFeature(negtiveDF,timegapSeq,featname) 
predictDF<-windowFeature(predictDF,timegapSeq,featname)
positiveDF[,"label"]<-1
negtiveDF[,"label"]<-0
alldata<-rbind(positiveDF,negtiveDF)
#join user info
setkey(alldata,user_id)
setkey(predictDF,user_id)
setkey(userInfo,user_id)
predictDF<-userInfo[predictDF]
alldata  <-userInfo[alldata]
 
setkey(alldata,targwindowday)
setkey(predictDF,targwindowday)
setkey(overallSTA,targwindowday)
predictDF<-overallSTA[predictDF]
alldata  <-overallSTA[alldata]
#load user embeddings
# user_embeddings<-fread('/Users/snakepointid/Documents/project/JDproj/save/UserEmbeddings.csv',header =T )
# names(user_embeddings)<-c(paste("user_embed",c(1:100),sep="_"),'user_id')
# setkey(alldata,user_id)
# setkey(predictDF,user_id)
# setkey(user_embeddings,user_id)
# predictDF<-user_embeddings[predictDF]
# alldata  <-user_embeddings[alldata]
# ##miss imp
# alldata   <- data.frame(alldata,   stringsAsFactors = F)
# predictDF <- data.frame(predictDF, stringsAsFactors = F)
# alldata   <- fillna(alldata,  0,   c("user_id","targwindowday"))
# predictDF <- fillna(predictDF,0,   c("user_id",'targwindowday'))
# 
# source('/Users/snakepointid/Documents/project/JDproj/hcmodel/constructModel.R')
