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

cate8featureConstruct<-function(cate,model_id,brand,type){
  model_id_times<-c(model_id,model_idMP)%>%table 
  type_times<-c(c(1:6),type)%>%table 
  brand_times<-c(brandMP,brand)%>%table 
  p<-which(cate==8)
  model_id_times8<-c(model_id[p],model_idMP)%>%table-1 
  type_times8<-c(c(1:6),type[p])%>%table-1 
  brand_times8<-c(brandMP,brand[p])%>%table-1 
  feat<-c(model_id_times8,model_id_times8/model_id_times,type_times8,type_times8/type_times,brand_times8,brand_times8/brand_times)
  return(paste(feat%>%round(.,3),collapse = "#"))
}

getFeat<-function(df,timegapSeq,featname){
  negdf<-df[,.(user_id)]%>%unique
  for(timegaps in timegapSeq){
    tmp<-df[timegap<timegaps,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(V1=cate8featureConstruct(cate,model_id,brand,type)),.(user_id)]
      tmp[,paste("timegap",timegaps,featname,sep='_')]<-do.call(rbind,tmp$V1%>%strsplit(.,"#"))%>%data.table
      tmp[,c('V1'):=NULL]
      setkey(negdf,user_id)
      setkey(tmp,user_id)
      negdf<-tmp[negdf]
    } 
  }
  return(negdf)
}