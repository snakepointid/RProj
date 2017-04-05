upperActionInfo<-actionInfo[,daysAction(model_id,type,cate,brand),.(user_id,day)]
featNums<-(upperActionInfo$V1[1]%>%strsplit(.,"#"))[[1]]%>%length
featName<-paste('daysActFeat',c(1:featNums),sep = "_")
tmp<-do.call(rbind,upperActionInfo$V1%>%strsplit(.,"#"))%>%data.table
upperActionInfo<-upperActionInfo[,c('V1'):=NULL]
names(tmp)<-featName
write.csv(tmp,"C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/rÏà¹Øº¯Êý/normaldnn/save/daysActivities.csv",row.names=F)
#upperActionInfo<-cbind(upperActionInfo,tmp)

 
