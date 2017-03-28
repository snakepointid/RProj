neglabel<-as.integer(-1)
ignoreLabel<-as.integer(-2)
getlabel<-function(sku_id,type,cate){
  p<-which(type=='order'&cate==8)
  if(p%>%length>0){return(sku_id[p])}
  p<-which(cate==8)
  if(p%>%length==0){return(neglabel)}
  return(ignoreLabel)
}
 