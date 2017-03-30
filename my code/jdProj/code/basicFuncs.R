 
usertimeAGGR<-function(sku_id,model_id,type,cate,brand,weekDate){
  maxMid<-table(model_id)%>%prop.table%>%round(.,2)
  maxMid<-maxMid[modid]
  maxMid[maxMid%>%is.na]<-0
  ret<-c(maxMid,typeMost(type,sku_id),typeMost(type,brand),typeMost(type,weekDate),typeCate(type,cate))
  return(paste(ret,collapse = "#"))
  }
typeret<-rep("unknow",6)
names(typeret)<-c("browse","buycar","cancel","order","star","hit")
 
typeMost<-function(type,module){
  ret<-typeret
  tmp<-table(type,module)
  ret[rownames(tmp)]<-colnames(tmp)[apply(tmp,1,which.max)]
  return(ret)
}
catelist<-c(8,5,6,4,7,11,9,10)
typecattable<-table(c("hit","hit",buytypeMP),catelist)
 
tcn<-sapply(catelist, function(s)paste(buytypeMP,s,sep = "_"))%>%as.vector
typecattable[,]<-0
typeCate<-function(type,cate){
  ret<-typecattable
  rettmp<-table(type,cate)%>%prop.table%>%round(.,2)
  ret[rownames(rettmp),colnames(rettmp)]<-rettmp
  return(ret%>%as.vector)
}
typeprop<-table(buytypeMP)
typeprop[]<-0
typeAggre<-function(type){
  buy<-0
  buycar<-0
  ret<-typeprop
  retmp<-table(type)
  if('order'%in%names(retmp))buy<-1
  if('buycar'%in%names(retmp))buycar<-1
  retmp<-retmp%>%prop.table%>%round(.,2)
  ret[names(retmp)]<-retmp
  return(paste(paste(ret,collapse = "#"),buycar,buy,sep="#"))
}
commentAGGR<-function(comment_num,bad_comment_rate){
  allcomt<-com_numMP[comment_num]
  badcomt<-(allcomt*bad_comment_rate)%>%round
  allcomt<-sum(allcomt)
  badcomt<-sum(badcomt)
  return(paste(allcomt,badcomt,sep="#"))
}

commentAGGR<-function(comment_num,bad_comment_rate){
  allcomt<-com_numMP[comment_num]
  badcomt<-(allcomt*bad_comment_rate)%>%round
  allcomt<-sum(allcomt)
  badcomt<-sum(badcomt)
  return(paste(allcomt,badcomt,sep="#"))
}
