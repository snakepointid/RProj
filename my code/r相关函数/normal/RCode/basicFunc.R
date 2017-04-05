  #construct mapping
#brand and modek_id top
model_idMP<-c(-2,-1,   0  ,26  ,27, 216 ,217)
brandMP   <-c(214,-1 ,306 ,403 ,489, 545, 800)
cateMP<-c(8,5,7,4,10,6,11,9)
typeMP<-c(1:6)
#one hot
oneHotEncode<-function(vec){
  hotcode<-diag(length(vec))%>%apply(.,1,function(x)paste(x,collapse="#"))
  names(hotcode)<-vec
  return(hotcode)
}
 
#
featureConstruct<-function(cate,model_id,brand,type){
  model_id_times<-c(model_id,model_idMP)%>%table 
  type_times<-c(typeMP,type)%>%table 
  brand_times<-c(brandMP,brand)%>%table 
  p<-which(cate==8)
  cate_times<-c(cateMP,cate)%>%table-1
  cate_prop<-cate_times%>%prop.table
  model_id_times8<-c(model_id[p],model_idMP)%>%table-1 
  type_times8<-c(typeMP,type[p])%>%table-1 
  brand_times8<-c(brandMP,brand[p])%>%table-1 
  feat<-c(model_id_times8,model_id_times,model_id_times8/model_id_times,
          type_times8,type_times,type_times8/type_times,
          brand_times8,brand_times,brand_times8/brand_times,
          cate_times,cate_prop)
  return(paste(feat%>%round(.,3),collapse = "#"))
}
 
windowFeature<-function(df,timegapSeq){
  featname<-c(paste("model_id8",c(model_idMP),"times",sep="_"),paste("model_idall",c(model_idMP),"times",sep="_"),paste("model_id8",c(model_idMP),"prop",sep="_"),
              paste("type8",typeMP,"times",sep="_"),paste("typeall",typeMP,"times",sep="_"),paste("type8",typeMP,"prop",sep="_"),
              paste("brand8",c(brandMP),"times",sep="_"),paste("brandall",c(brandMP),"times",sep="_"),paste("brand8",c(brandMP),"prop",sep="_"),
              paste("cate",c(cateMP),"times",sep="_"),paste("cate",c(cateMP),"prop",sep="_"))
  negdf<-df[,.(user_id,targwindowday,label)]%>%unique
  for(timegaps in timegapSeq){
    tmp<-df[timegap<=timegaps,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(V1=featureConstruct(cate,model_id,brand,type)),.(user_id,targwindowday)]
      tmp[,paste("timegap",timegaps,featname,sep='_')]<-do.call(rbind,tmp$V1%>%strsplit(.,"#"))%>%data.table
      tmp[,c('V1'):=NULL]
      setkey(negdf,user_id,targwindowday)
      setkey(tmp,user_id,targwindowday)
      negdf<-tmp[negdf]
    } 
  }
  return(negdf)
}
#
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
  
getWindayInfoWraper<-function(df){
  featname<-c(paste("type",typeMP,"times",sep="_"),paste("type",typeMP,"prop",sep="_"),
              paste("cate",cateMP,"times",sep="_"),paste("cate",cateMP,"prop",sep="_"))
  overallSTA<-df[,getWindayInfo(type,cate),.(windowday)]
  overallSTA[,featname]<-do.call(rbind,overallSTA$V1%>%strsplit(.,"#"))%>%data.table
  overallSTA<-overallSTA[,c('V1'):=NULL]
  return(overallSTA)
}
getWindayInfo<-function(type,cate){
  catetype<-paste(cate,type,sep="#")
  catetimes<-c(cateMP,cate)%>%table-1
  typetimes<-c(typeMP,type)%>%table-1
  cateProp<-catetimes%>%prop.table%>%round(.,2)
  typeProp<-typetimes%>%prop.table%>%round(.,2)
  feat<-c(typetimes,typeProp,catetimes,cateProp)
  return(paste(feat,collapse = "#"))
}

# typeCode<-function(type){
#   return(paste(table(c(1:6,type))-1,collapse="#"))
# }

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
  negdf<-df[,.(user_id,targwindowday,label)]%>%unique
  for(timegaps in timegapSeq){
    tmp<-df[timegap<=timegaps,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(pop=findActMostItem(popitem)),.(user_id,targwindowday)]
      names(tmp)<-c("user_id",paste("timegap",timegaps,"mostactitem",sep="_"))
      setkey(negdf,user_id,targwindowday)
      setkey(tmp,user_id,targwindowday)
      negdf<-tmp[negdf]
    } 
    tmp<-df[timegap<=timegaps&cate==8,]
    if(tmp$user_id%>%length>0){
      tmp<-tmp[,.(pop=findActMostItem(popitem)),.(user_id,targwindowday)]
      names(tmp)<-c("user_id",paste("timegap",timegaps,"mostactitem",sep="_"))
      setkey(negdf,user_id,targwindowday)
      setkey(tmp,user_id,targwindowday)
      negdf<-tmp[negdf]
    } 
  }
  return(negdf)
}