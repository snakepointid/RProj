#match the extracted text with the crawled jd class file 

getBrandItem<-function(matBrand,matItem){
  brandIdx<- which(itemClassSets$品牌%in%matBrand)
  if(brandIdx%>%length==0)
    return(NA)else if(brandIdx%>%length==1)
      return(brandIdx)
  matItem <- paste(matItem%>% unique(),collapse = '|')
  matIdx  <- which(str_detect(itemClassSets$分类[brandIdx],matItem))
  if(matIdx%>%length==0)
    return(brandIdx[1])else 
      return(brandIdx[matIdx[1]])
}
#get most frequent category and its num and proporation
getVecFeat<-function(vec){
  if(vec[!vec%>%is.na]%>%length==0)
    return(paste(rep('NA',3),collapse = "#"))
  tb  <-vec%>%table
  tbp <-tb%>%prop.table%>%round(.,2)
  tbn <-tb%>%names
  wmx <-tb%>%which.max
  return(paste(c(tbn[wmx],tbp[wmx],tb[wmx]),collapse = "#"))
}
#get buyed item types
getBuyedFeat<-function(first,second,third,brand,color){
  feat<-NULL
  for(cl in list(first,second,third,brand,color)){
    if(is.null(feat))
      feat<-getVecFeat(cl)else
        feat<-paste(feat,getVecFeat(cl),sep="#")
  }
  return(feat)
}
#get address
getAddFeat<-function(recvier,prov,City,Xian,Zhen,Cun){
  jd_diffReceiver_num<-recvier%>%unique%>%length
  jd_diffProv_num   <-prov%>%unique%>%length
  jd_diffCity_num   <-City%>%unique%>%length
  prov_feat         <-getVecFeat(prov)  
  city_feat         <-getVecFeat(City) 
  
  allNum <-Xian%>%length
  XianNum<-Xian[Xian=='县']%>%length
  ZhenNum<-Zhen[Zhen=='镇']%>%length
  CunNum <-Cun[Cun=='村']%>%length
  
  jd_xian_has       <-as.numeric(XianNum/allNum>0.5)
  jd_zhen_has       <-as.numeric(ZhenNum/allNum>0.5)
  jd_cun_has        <-as.numeric(CunNum /allNum>0.5)
  return(paste(c(jd_diffReceiver_num,jd_diffProv_num,jd_diffCity_num,prov_feat,city_feat,jd_xian_has,jd_zhen_has,jd_cun_has),collapse ="#"))
}
#get money feat
getOrderFeat<-function(order_time){
  order_time<-order_time%>%sort%>%diff
  avgPay <-order_time%>%mean(.,na.rm=T)%>%round(.,2)
  stdPay <-order_time%>%sd(.,na.rm=T)%>%round(.,2)
  return(paste(avgPay,stdPay,sep='#'))
} 
#dplit df
splitTheFeature<-function(DT,var='V1',featName=NULL){
  tmp<-do.call(rbind,DT[,eval(parse(text=var))]%>%str_split(.,"#"))%>%data.table
  if(!featName%>%is.null)names(tmp)<-featName
  DT<-cbind(DT,tmp)
  DT[,c(var):=NULL]
  return(DT)
}
#get bank feat

getBankFeat<-function(bankN,tailN,cardT,ownerN,phoneN){
  bankTB      <-paste(table(c(bankN,DFT_bankName))[DFT_bankName]-1,collapse = '#')
  cardTB      <-paste(table(c(cardT,DFT_card_type))[DFT_card_type]-1,collapse = '#')
  bankFeat    <-getVecFeat(bankN)
  cardFeat    <-getVecFeat(cardT)
  diffCardNum <-tailN%>%unique%>%length
  diffOwnNum  <-ownerN%>%unique%>%length
  diffphoneNum<-phoneN%>%unique%>%length
  return(paste(bankTB,cardTB,bankFeat,cardFeat,diffCardNum,diffOwnNum,diffphoneNum,sep='#'))
}
#basic category var FE
cateVarFE<-function(DT,RDT,var,idkey,name,DFT=NULL,maxCate=T){
  if(DT[,eval(parse(text=idkey))]%>%length==0)
    return(RDT)
  #add default
  #redefine the data type to characters
  DT[,var]<-DT[,eval(parse(text=var))]%>%as.character
  #overall statistic
  if(maxCate){
    NDT<-DT[,getVecFeat(eval(parse(text=var))),.(eval(parse(text=idkey)))]
    names(NDT)<-c(idkey,'V1')
    NDT<-splitTheFeature(NDT,'V1',paste(name,c('Most_cate','Most_prop','Most_num'),sep=''))
    RDT<-merge(RDT,NDT,by=idkey,all = T)
  }
  if(DFT%>%is.null)return(RDT)
  NDT<-DT[,c(var,idkey),with=FALSE]
  NDT<-NDT[eval(parse(text=var))%in%DFT]
  NDT<-dcast(NDT,eval(parse(text=paste(idkey,var,sep='~'))),fun=length,value.var = idkey)
  FeatName<-NDT%>%names
  FeatName<-c(FeatName[1],paste(name,'Is',FeatName[-1],'_num',sep=""))
  names(NDT)<-FeatName
  RDT<-merge(RDT,NDT,by=idkey,all = T)
  return(RDT)
}


timeFeat<-function(time){
  time<-sort(time)
  times<-time%>%length
  span<-max(time)-min(time)
  diff<-diff(time)
  if(diff%>%length<2)return(paste(times,span,'-1#-1#-1',sep='#'))
  minGap<-min(diff)
  maxGap<-max(diff)
  avgGap<-mean(diff)%>%round(.,0)
  return(paste(times,span,minGap,maxGap,avgGap,sep='#'))
}
#basic date var FE
dateVarFE<-function(DT,RDT,datevar,idkey,name){
  if(DT[,eval(parse(text=idkey))]%>%length==0)
    return(RDT)
  #redefine the data type to characters
  DT[,datevar]<-DT[,eval(parse(text=datevar))]%>%as.character
  #mapping time
  DT[,'timeMap']<-(DT[,eval(parse(text = datevar))]%>%str_sub(.,1,4)%>%as.numeric-2017)*365+
    DT[,eval(parse(text = datevar))]%>%str_sub(.,6,7)%>%as.numeric*30+DT[,eval(parse(text = datevar))]%>%str_sub(.,9,10)%>%as.numeric 
  NDT<-DT[,.(timeFeat(timeMap)),.(eval(parse(text = idkey)))]
  NDT<-splitTheFeature(NDT,'V1')
  names(NDT)<-c(idkey,paste(name,c('times_num','Time_span','TimeGap_min','TimeGap_max','TimeGap_avg'),sep=''))
  RDT<-merge(RDT,NDT,by=idkey,all = T)
  return(RDT)
}

#basic num var FE
numVarFE<-function(DT,RDT,var,idkey,name){
  if(DT[,eval(parse(text=idkey))]%>%length==0)
    return(RDT)
  #redefine the data type to characters
  DT[,var]<-DT[,eval(parse(text=var))]%>%as.character%>%as.numeric
  #num statistic
  NDT<-dcast(DT,eval(parse(text=paste(idkey,'~.',sep=''))),fun=list(max,min,sum,mean),value.var = var)
  names(NDT)<-c(idkey,paste(name,c('max','min','sum','avg'),sep='_'))
  RDT<-merge(RDT,NDT,by=idkey,all = T)
  return(RDT)
}

#cate seg num faeture
cateNumIntFE<-function(DT,RDT,catevar,numvar,idkey,name1,name2,DFT){
  if(DT[,eval(parse(text=idkey))]%>%length==0)
    return(RDT)
  DT<-DT[,c(catevar,numvar,idkey),with=FALSE]
  #redefine the data type to characters
  DT[,numvar] <-DT[,eval(parse(text=numvar))]%>%as.character%>%as.numeric
  DT[,catevar]<-DT[,eval(parse(text=catevar))]%>%as.character 
  #set defalut
  DFDT<-data.table(DFT,-1,'-1')
  names(DFDT)<-c(catevar,numvar,idkey)
  DT<-rbind(DT[eval(parse(text=catevar))%in%DFT],DFDT)
  #num statistic
  NDT<-dcast(DT,eval(parse(text=paste(idkey,'~',catevar,sep=''))),fun=list(max,min,sum,mean),value.var = numvar)
  featName<-setFeatNames(list(c(name1),paste(name2,DFT%>%sort,sep=''),c('max','min','sum','avg')),c(1,2,3))

  for(i in names(NDT)){
    vec<-NDT[,eval(parse(text=i))]
    vec[is.infinite(vec)|is.na(vec)|is.nan(vec)]<--1
    NDT[,i]<-vec
  }
  names(NDT)<-c(idkey,featName)
  RDT<-merge(RDT,NDT,by=idkey,all = T)
  return(RDT)
}
#mapping the date
dateMap<-function(date){
  date<-date%>%str_sub(.,1,10)
  uniqueDate<-date%>%unique%>%as.Date
  names(uniqueDate)<-uniqueDate
  return(uniqueDate[date])
}
 
#date seg num faeture
dateNumIntFE<-function(DT,RDT=NULL,datevar='dt',numvar='fakevar',idkey='fakevar',timeGaps='fakevar',FeatName='fakevar'){
  if(DT[,eval(parse(text=idkey))]%>%length==0)
    return(RDT)
  DT<-DT[,c(datevar,numvar,idkey),with=FALSE]
  #now time
  NOW<-Sys.time()%>%as.Date 
  #rec early
  recDT<-onePerOneRecord(DT,idkey,datevar)
  recDT[,'v1']<-NOW-dateMap(recDT[,eval(parse(text = datevar))])
  recDT[,'v2']<- recDT[,eval(parse(text=numvar))]
  firDT<-onePerOneRecord(DT,idkey,datevar,F)
  firDT[,'v1']<- firDT[,eval(parse(text=numvar))]
  recDT<-recDT[,c(idkey,'v1','v2'),with=FALSE]
  firDT<-firDT[,c(idkey,'v1'),with=FALSE]
  names(recDT)<-c(idkey,paste(FeatName,c('Silence_days','Rec_value'),sep=''))
  names(firDT)<-c(idkey,paste(FeatName,'First_value',sep=''))
  #overall statistic
  NDT<-dcast(DT,eval(parse(text=idkey))~.,fun=list(max,min,sum),value.var = numvar)
  names(NDT)<-c(idkey,paste(FeatName,'Overall',c('_max','_min','_sum'),sep=''))
  if(RDT%>%is.null)
    RDT<-NDT else
      RDT<-merge(RDT,NDT,idkey)
  #merge
  RDT<-merge(RDT,recDT,by=idkey)
  RDT<-merge(RDT,firDT,by=idkey)
  for(timegap in timeGaps){
    timeThreshold<-NOW-timegap
    SubDT <- DT[eval(parse(text=datevar))>timeThreshold]
    if(SubDT[,eval(parse(text=datevar))]%>%length) 
      NDT   <-dcast(SubDT,eval(parse(text=idkey))~.,fun=list(max,min,sum,mean),value.var = numvar)else
        NDT<-data.table('-1','-1','-1','-1','-1')
    names(NDT)<-c(idkey,paste(FeatName,'Rec',timegap,'day',c('_max','_min','_sum','_avg'),sep=''))
    RDT<-merge(RDT,NDT,by=idkey,all.x = T)
  }
  return(RDT)
}
