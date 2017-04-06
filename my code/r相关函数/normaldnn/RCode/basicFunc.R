  #construct mapping
#brand and modek_id top

#one hot
oneHotEncode<-function(vec){
  hotcode<-diag(length(vec))%>%apply(.,1,function(x)paste(x,collapse="#"))
  names(hotcode)<-vec
  return(hotcode)
}
#impute the missing values
fillna<-function(df,value,ignore){
  for(i in names(df)){
    if(i%in%ignore)next
    p<-which(df[,i]%>%is.na|df[,i]%>%is.nan|df[,i]=="NA")
    df[p,i]<-value
    df[,i]<-df[,i]%>%as.numeric
  }
  return(df)
}
#get days actions 
 
daysAction<-function(model_id,type,cate,brand){
  model_id_all_prop<-(c(model_id_ALL,model_id)%>%table-1)%>%prop.table%>%round(.,3)
  type_all_prop<-(c(type_ALL,type)%>%table-1)%>%prop.table%>%round(.,3)
  cate_all_prop<-(c(cate_ALL,cate)%>%table-1)%>%prop.table%>%round(.,3)
  brand_all_prop<-(c(brand_ALL,brand)%>%table-1)%>%prop.table%>%round(.,3)
 #specific cate 8
  cate8p<-which(cate==8)
  if(cate8p%>%length>0){
    model_id_8_prop<-(c(model_id_ALL,model_id[cate8p])%>%table-1)%>%prop.table%>%round(.,3)
    type_8_prop<-(c(type_ALL,type[cate8p])%>%table-1)%>%prop.table%>%round(.,3)
    brand_8_prop<-(c(brand_ALL,brand[cate8p])%>%table-1)%>%prop.table%>%round(.,3)
  }else{
    model_id_8_prop<-rep(0,model_id_ALL%>%length)
    type_8_prop<-rep(0,type_ALL%>%length)
    brand_8_prop<-rep(0,brand_ALL%>%length)
  }
  
  feat<-c(model_id_all_prop,model_id_8_prop,
          type_all_prop,type_8_prop,
          brand_all_prop,brand_8_prop,
          cate_all_prop)
  return(paste(feat,collapse = "#"))
}

