neglabel<-as.integer(-1)
ignoreLabel<-as.integer(-2)
getlabel<-function(sku_id,type,cate){
  p<-which(type=='order'&cate==8)
  if(p%>%length>0){return(sku_id[p])}
  p<-which(cate==8|type=='order')
  if(p%>%length==0){return(neglabel)}
  return(ignoreLabel)
}
 
oneHotEncode<-function(vec){
  hotcode<-diag(length(vec))%>%apply(.,1,function(x)paste(x,collapse="#"))
  names(hotcode)<-vec
  return(hotcode)
}