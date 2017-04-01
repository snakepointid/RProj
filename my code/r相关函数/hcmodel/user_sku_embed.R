PosiUserItemPair<-allActionInfo[type==4,.(user_id,sku_id)] 
itemList<-PosiUserItemPair$sku_id 
itemSelectProp<-itemList%>%table%>%prop.table() 
itemList<-itemList%>%unique
 
NegBuyedUserItemPair<-PosiUserItemPair[,.(sku_id=findNegUserItemPair(sku_id,itemList,itemSelectProp,100)),.(user_id)]
for(i in c(1:4)){
  PosiUserItemPair<-rbind(PosiUserItemPair,PosiUserItemPair)
}
 
NegBuyedUserItemPair[,'label']<-0
PosiUserItemPair[,'label']<-1
UserItemPair<-rbind(NegBuyedUserItemPair,PosiUserItemPair)
sampNum<-UserItemPair$user_id%>%length
UserItemPair<-UserItemPair[sample(1:sampNum,sampNum,replace = F),]
write.csv(UserItemPair,file = "C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/jdProj/save/UserItemPair.csv",row.names = F)
 
 
UserItemPair$label%>%table%>%prop.table
 