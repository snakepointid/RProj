library(data.table)
library(dplyr)
#maping docs
sexMP<-c("male","female","secret")
names(sexMP)<-c(0,1,2)
com_numMP<-c(0,1,5,25,100)
names(com_numMP)<-c(0,1,2,3,4)
buytypeMP<-c("browse","buycar","cancel","order","star","hit")
names(buytypeMP)<-c(1,2,3,4,5,6)
source('~/Documents/project/RProj/my code/jdProj/code/basicFuncs.R')
#read date
maxrows=-1
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
actionInfo[,'type']<-actionInfo[,buytypeMP[type]]
##date cut 
actionInfo[,"cutDate"]<-actionInfo[,substr(time,1,10)]
date<-actionInfo$cutDate%>%unique%>%as.Date
earlday<-min(date)-1
lastday<-as.Date("2016-04-15")
date<-seq(earlday,lastday,1)
cutday<-sort(date,decreasing = T)
weekday<-cutday%>%weekdays
names(weekday)<-cutday
actionInfo[,"weekDate"]<-actionInfo[,weekday[cutDate]]
vl<-cutday%>%length
cutp<-seq(1,vl,5)
cutdayMP<-rep(cutday[cutp],each=5)[1:vl]
names(cutdayMP)<-cutday
#set lack day
hasday<-table(cutdayMP,weekday)
actionInfo[,"cutDate"]<-actionInfo[,cutdayMP[cutDate]]
#user time aggregation
modid<-actionInfo$model_id%>%table
modid<-names(sort(modid,decreasing = T))[1:10]
if(is.null(modid))modid<-c("0")
fname<-c(paste(modid,"prop",sep="_"),paste(names(typeret),"product",sep="_most_")
         ,paste(names(typeret),"brand",sep="_most_")
         ,paste(names(typeret),"weekday",sep="_most_")
         ,tcn
          )

usertimeInfo<-actionInfo[,usertimeAGGR(sku_id,model_id,type,cate,brand,weekDate),keyby=.(user_id,cutDate)]  
usertimeInfo[,fname]<-do.call(rbind,usertimeInfo$V1%>%strsplit(.,"#"))%>%data.table
usertimeInfo[,c('V1'):=NULL]

#final df
distinctT<-cutdayMP%>%unique
finalDF<-actionInfo[type=="order",.(user_id,sku_id)] 
finalDF<-finalDF[,.(cutDate=distinctT),.(user_id,sku_id)]
#user target
setkey(actionInfo,user_id,sku_id,cutDate)
setkey(finalDF,user_id,sku_id,cutDate)
subactionInfo<-merge(actionInfo,finalDF)
user_item_time_info<-subactionInfo[,typeAggre(type),by=.(user_id,sku_id,cutDate)] 
fname<-c(paste(buytypeMP,"prop",sep="_"),"put2car","orderitem")
user_item_time_info[,fname]<-do.call(rbind,user_item_time_info$V1%>%strsplit(.,"#"))%>%data.table
user_item_time_info[,c('V1'):=NULL]
targInfo<-user_item_time_info[orderitem==1,.(user_id,sku_id,cutDate=cutDate-5)]
targInfo[,"futurebuy"]<-1
setkey(finalDF,user_id,sku_id,cutDate)
setkey(targInfo,user_id,sku_id,cutDate)
finalDF<-targInfo[finalDF ]

targprodlist<-productInfo$sku_id%>%unique
targprodlist<-targprodlist[targprodlist%in%targInfo$sku_id]
addDF<-finalDF[,(which(sku_id%in%targprodlist)%>%length<1)%>%as.numeric,.(user_id,cutDate)]
addDF[,'sku_id']<--1
addDF<-addDF[,.(futurebuy=V1,user_id,cutDate,sku_id)]
finalDF<-rbind(finalDF,addDF)
finalDF[futurebuy%>%is.na,'futurebuy']<-0
#user info
userInfo$user_reg_dt<-(as.Date('2017-03-25')-userInfo$user_reg_dt%>%as.Date)%>%as.numeric
#coment info
commentInfo<-commentInfo[sku_id%in%finalDF$sku_id,]
commentInfo[,"cutDate"]<-commentInfo[,cutdayMP[dt]]
commentInfo[,'comment_num']<-commentInfo[,as.character(comment_num)] 
commentInfo<-commentInfo[,commentAGGR(comment_num,bad_comment_rate),keyby=.(sku_id,cutDate)]  
commentInfo[,c("allcomt","badcomt")]<-do.call(rbind,commentInfo$V1%>%strsplit(.,"#"))%>%data.table
commentInfo<-commentInfo[,.(allcomt=cumsum(allcomt),badcomt=cumsum(badcomt),cutDate=cutDate),by=.(sku_id)] 
commentInfo[,"badrat"]<-commentInfo[,badcomt/(allcomt+1)]%>%round(.,2)
commentInfo[,'allcomt']<-commentInfo[,'allcomt']/commentInfo[,'allcomt']%>%max
commentInfo[,'badcomt']<-commentInfo[,'badcomt']/commentInfo[,'badcomt']%>%max
##merge
##df need to merge: finalDF,usertimeInfo,user_item_time_info,targInfo,userInfo,commentInfo,productInfo
setkey(finalDF,user_id,cutDate)
setkey(usertimeInfo,user_id,cutDate)
finalDF<-usertimeInfo[finalDF ]

setkey(finalDF,user_id,sku_id,cutDate)
setkey(user_item_time_info,user_id,sku_id,cutDate)
finalDF<-user_item_time_info[finalDF ]



setkey(finalDF,user_id )
setkey(userInfo,user_id)
finalDF<-userInfo[finalDF ]

setkey(finalDF,sku_id,cutDate)
setkey(commentInfo,sku_id,cutDate)
finalDF<-commentInfo[finalDF ]

setkey(finalDF,sku_id)
setkey(productInfo,sku_id)
finalDF<-productInfo[finalDF ]
targProdList<-finalDF[sku_id%in%productInfo$sku_id,.(sku_id)]%>%unique()
finalDF<-finalDF[sku_id!=-1,]
subFinalDF<-finalDF[1:100000,]
fwrite(finalDF,file = "/Users/snakepointid/Documents/project/RProj/my code/jdProj/save/finalDF.csv")
fwrite(subFinalDF,file = "/Users/snakepointid/Documents/project/RProj/my code/jdProj/save/subFinalDF.csv")
fwrite(targProdList,file = "/Users/snakepointid/Documents/project/RProj/my code/jdProj/save/targProdList.csv")
