# PosiUserItemPair<-actionInfo[type==4,.(user_id,sku_id)] 
# itemList<-PosiUserItemPair$sku_id 
# itemSelectProp<-itemList%>%table%>%prop.table() 
# itemList<-itemList%>%unique
# 
# NegBuyedUserItemPair<-PosiUserItemPair[,.(sku_id=findNegUserItemPair(sku_id,itemList,itemSelectProp,100)),.(user_id)]
# for(i in c(1:4)){
#   PosiUserItemPair<-rbind(PosiUserItemPair,PosiUserItemPair)
# }

# NegBuyedUserItemPair[,'label']<-0
# PosiUserItemPair[,'label']<-1
# UserItemPair<-rbind(NegBuyedUserItemPair,PosiUserItemPair)
# sampNum<-UserItemPair$user_id%>%length
# UserItemPair<-UserItemPair[sample(1:sampNum,sampNum,replace = F),]
# write.csv(UserItemPair,file = "/Users/snakepointid/Documents/project/JDproj/save/UserItemPair.csv",row.names = F)
buyedItems<-actionInfo[type==4,]$sku_id%>%unique
PosiUserItemPair<-actionInfo[sku_id%in%buyedItems&(!(cate==8&type==4)),.(user_id,sku_id,type)] 
PosiUserItemPair<-PosiUserItemPair[,.(nums=.N),.(user_id,sku_id,type)]
PosiUserItemPair[,'nums']<-PosiUserItemPair$nums%>%log(.,exp(2)) 
 
typeOnehot<-c(1:6)%>%oneHotEncode
PosiUserItemPair[,paste("type",c(1:length(typeOnehot)),sep="_")]<-do.call(rbind,typeOnehot[PosiUserItemPair$type]%>%strsplit(.,"#"))%>%data.table
PosiUserItemPair[,c('type'):=NULL]
sampNum<-PosiUserItemPair$user_id%>%length
samps<-sample(c(1:sampNum),sampNum,replace = F)
PosiUserItemPair<-PosiUserItemPair[samps,]
write.csv(PosiUserItemPair,file = "/Users/snakepointid/Documents/project/JDproj/save/UserItemPairNums.csv",row.names = F)
 
