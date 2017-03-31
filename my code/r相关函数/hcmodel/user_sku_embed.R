buyedSku<-allActionInfo[cate==8&type==4,]$sku_id%>%unique
userActionHistory<-allActionInfo[sku_id%in%buyedSku ,]
userActionHistory<-userActionHistory[,1,.(user_id,sku_id,type)]
ruleRejctUser<-allActionInfo[cate==8&type==4,]$user_id%>%unique

 
userActionHistory<-userActionHistory[,typeCode(type),.(user_id,sku_id)]
sampNum<-length(userActionHistory$user_id)
userActionHistory<-userActionHistory[sample(1:sampNum,sampNum,replace = F),]
write.csv(userActionHistory,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/userActionHistory.csv",row.names = F)
 
predUser<-userActionHistory$user_id%>%unique
predUser<-predUser[!predUser%in%ruleRejctUser]
predDF<-data.table(user_id=predUser)
predDF<-predDF[,buyedSku,.(user_id)]

write.csv(predDF,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/predDF.csv",row.names = F)

