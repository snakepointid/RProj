# upperActionInfo<-actionInfo[,daysAction(model_id,type,cate,brand),.(user_id,day)]
# featNums<-(upperActionInfo$V1[1]%>%strsplit(.,"#"))[[1]]%>%length
# featName<-paste('daysActFeat',c(1:featNums),sep = "_")
# tmp<-do.call(rbind,upperActionInfo$V1%>%strsplit(.,"#"))%>%data.table
# upperActionInfo<-upperActionInfo[,c('V1'):=NULL]
# names(tmp)<-featName
# upperActionInfo<-cbind(upperActionInfo,tmp)
# write.csv(upperActionInfo,"C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/save/daysActivities.csv",row.names=F)
#  

daysActivisCode<-fread("C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/save/daysActivities_prop_coding.csv",header = T) 
defaultCode<-fread("C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/save/daysActivities_prop_default_coding.csv",header = T) 
daySpan<-daysActivisCode$day%>%unique
daysActivisCodeAll<-daysActivisCode[,.(day=daySpan),.(user_id)]

setkey(daysActivisCode,user_id,day) 
setkey(daysActivisCodeAll,user_id,day) 
daysActivisCodeAll<-daysActivisCode[daysActivisCodeAll]
 
daysActivisCodeAll<-daysActivisCodeAll%>%as.data.frame(.,stringAsFactor=F)
missp<-which(daysActivisCodeAll$`0`%>%is.na)
daysActivisCodeAll[missp,c(1:10)]<-defaultCode[1,]
daysActivisCodeAll<-daysActivisCodeAll%>%data.table

dayMP<-daySpan%>%as.Date
names(dayMP)<-daySpan
UserTargdayLabel<-merge(daysActivisCodeAll, UserTargdayLabel, by.x = 'user_id', by.y = 'user_id',allow.cartesian=T)
UserTargdayLabel[,'day']<-dayMP[UserTargdayLabel$day]
UserTargdayLabel[,'daygap']<-UserTargdayLabel$targwindowday-UserTargdayLabel$day
UserTargdayLabel<-UserTargdayLabel[daygap<31&daygap>0,]
UserTargdayLabel<-dcast(UserTargdayLabel, user_id +targwindowday+label ~ daygap, value.var = c("1",'2','3','4','5','6','7','8','9'))
sampNum<-UserTargdayLabel$user_id%>%length
samp<-sample(c(1:sampNum),sampNum,replace = F)
UserTargdayLabel<-UserTargdayLabel[samp,]
UserTargdayLabel
#clean mem
write.csv(UserTargdayLabel,"C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/save/UserTargdayLabel.csv",row.names=F)
