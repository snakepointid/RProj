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
 
 
featureConstruct<-function(cate,model_id,brand,type){
  model_id_times<-c(model_id,model_idMP)%>%table 
  type_times<-c(c(1:6),type)%>%table 
  brand_times<-c(brandMP,brand)%>%table 
  p<-which(cate==8)
  
  model_id_times8<-c(model_id[p],model_idMP)%>%table-1 
  type_times8<-c(c(1:6),type[p])%>%table-1 
  brand_times8<-c(brandMP,brand[p])%>%table-1 
  feat<-c(model_id_times8,model_id_times,model_id_times8/model_id_times,type_times8,type_times,type_times8/type_times,brand_times8,brand_times,brand_times8/brand_times)
  return(paste(feat%>%round(.,3),collapse = "#"))
}
 
windowFeature<-function(df,timegapSeq,featname){
  negdf<-df[,.(user_id,targwindowday)]%>%unique
  for(timegaps in timegapSeq){
    tmp<-df[timegap<timegaps,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(V1=featureConstruct(cate,model_id,brand,type)),.(user_id)]
      tmp[,paste("timegap",timegaps,featname,sep='_')]<-do.call(rbind,tmp$V1%>%strsplit(.,"#"))%>%data.table
      tmp[,c('V1'):=NULL]
      setkey(negdf,user_id)
      setkey(tmp,user_id)
      negdf<-tmp[negdf]
    } 
  }
  return(negdf)
}
findSku<-function(sku_id){
  tb<-sku_id%>%table
  if(tb%>%length>0){
    return(names(tb)[which.max(tb)]%>%as.integer)
  }
  return(NA)
}

fillna<-function(df,value,ignore){
  for(i in names(df)){
    if(i%in%ignore)next
    p<-which(df[,i]%>%is.na|df[,i]%>%is.nan|df[,i]=="NA")
    df[p,i]<-value
    df[,i]<-df[,i]%>%as.numeric
  }
  return(df)
}
 
typeValue<-c(1:6)
cateValue<-c(8,5,6,4,7,11,9,10)
cateTypeValue<-c()
for(typev in typeValue){
  cateTypeValue<-c(cateTypeValue,paste(cateValue,typev,sep="#"))
}

getWindayInfoWraper<-function(df){
  featname<-c(paste("type",typeValue,"times",sep="_"),paste("type",typeValue,"prop",sep="_"),
              paste("cate",cateValue,"times",sep="_"),paste("cate",cateValue,"prop",sep="_"),
              paste("cateType",cateTypeValue,"times",sep="_"),paste("cateType",cateTypeValue,"prop",sep="_"))
  overallSTA<-df[,getWindayInfo(type,cate),.(windowday)]
  overallSTA[,featname]<-do.call(rbind,overallSTA$V1%>%strsplit(.,"#"))%>%data.table
  overallSTA<-overallSTA[,c('V1'):=NULL]
  return(overallSTA)
}
getWindayInfo<-function(type,cate){
  catetype<-paste(cate,type,sep="#")
  catetimes<-c(cateValue,cate)%>%table-1
  typetimes<-c(typeValue,type)%>%table-1
  catetypetimes<-c(cateTypeValue,catetype)%>%table-1
  cateProp<-catetimes%>%prop.table%>%round(.,2)
  typeProp<-typetimes%>%prop.table%>%round(.,2)
  catetypeProp<-catetypetimes%>%prop.table%>%round(.,2)
  feat<-c(typetimes,typeProp,catetimes,cateProp,catetypetimes,catetypeProp)
  return(paste(feat,collapse = "#"))
}

typeCode<-function(type){
  return(paste(table(c(1:6,type))-1,collapse="#"))
}

findNegUserItemPair<-function(buyedList,itemList,itemSelectProp,k){
  samp<-sample(itemList,k,replace = F,prob = itemSelectProp)
  samp<-samp[!samp%in%buyedList]
  samp%>%return
}

findActMostItem<-function(items){
  tb<-items%>%table
  (tb%>%names)[which.max(tb)]%>%as.integer%>%return
}

findActMostItemBytime<-function(df,timegapSeq){
  negdf<-df[,.(user_id)]%>%unique
  
  for(timegaps in timegapSeq){
    tmp<-df[timegap<=timegaps,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(pop=findActMostItem(popitem)),.(user_id)]
      names(tmp)<-c("user_id",paste("timegap",timegaps,"mostactitem",sep="_"))
      setkey(negdf,user_id)
      setkey(tmp,user_id)
      negdf<-tmp[negdf]
    } 
  }
  return(negdf)
}