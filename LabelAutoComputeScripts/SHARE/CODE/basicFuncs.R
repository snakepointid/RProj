#load data
showDATA<-function(TBname,dtkey){
  ch <- odbcConnect("impalaodbc", uid = "changlue.she", pwd = "9f671e71a33b3454b9593d4f32f6ff18")
  sqlTables(ch)
  batchDay<-(Sys.time()%>%as.Date-1)%>%as.character
  sql<- paste("select count(1) from ",TBname,' where substr(',dtkey,",1,10)='",batchDay,"'"," or substr(",dtkey,",1,8)='",batchDay%>%str_replace_all(.,"-",""),"'",sep="")
  dataNum <- sqlQuery(ch,sql,as.is=T)
  
  sql<- paste("select * from ",TBname,' where substr(',dtkey,",1,10)='",batchDay,"'"," or substr(",dtkey,",1,8)='",batchDay%>%str_replace_all(.,"-",""),"' limit 1000",sep="")
  DT <- sqlQuery(ch,sql,as.is=T)%>%data.table
  odbcClose(ch)

  print(paste('the sample nums:',dataNum))
  return(DT)
}
 
loadDATA<-function(fields,idkey,dtkey,TBname,lasteUpdate='2017-05-11',limit='limit 100',augment=F,constraint=''){
  ch <- odbcConnect("impalaodbc", uid = "changlue.she", pwd = "9f671e71a33b3454b9593d4f32f6ff18")
  sqlTables(ch)
  batchDay<-(Sys.time()%>%as.Date-1)%>%as.character
  fields  <- paste(fields,collapse = ',')
  if(fields=='#ALL#')
    sql     <- paste("select * from ",TBname,sep='')else
      sql     <- paste("select ",fields,',',idkey,',',dtkey," from ",TBname,sep='')
  if(!augment)sql<-paste(sql,' where (substr(',dtkey,",1,10)<='",batchDay,"' and substr(",dtkey,",1,10)>='",lasteUpdate,"') or (substr(",dtkey,",1,8)<='",batchDay%>%str_replace_all(.,"-",""),"' and substr(",dtkey,",1,8)>='",lasteUpdate%>%str_replace_all(.,"-",""),"')",sep="")
  #if(!augment)sql<-paste(sql,' where substr(',dtkey,",1,10)='",batchDay,"'"," or substr(",dtkey,",1,8)='",batchDay%>%str_replace_all(.,"-",""),"'",sep="")
  if(augment) sql<-paste(sql,' where ',idkey,' in (select ',idkey,' from ',TBname,' where (substr(',dtkey,",1,10)<='",batchDay,"' and substr(",dtkey,",1,10)>='",lasteUpdate,"') or (substr(",dtkey,",1,8)<='",batchDay%>%str_replace_all(.,"-",""),"' and substr(",dtkey,",1,8)>='",lasteUpdate%>%str_replace_all(.,"-",""),"'))",sep="")
  sql     <- paste(sql,constraint,limit)
  DT      <- sqlQuery(ch,sql,as.is=T)
  odbcClose(ch)
  DT      <-DT%>%data.table
  if(!augment)DT<-uniqueData(DT,idkey,dtkey)
  DT[,idkey]<-str_replace_all(DT[ ,eval(parse(text=idkey))],'\\.','')
  return(DT)
}
uniqueData<-function(DT,idkey,dtkey){
  GDT<-DT[,.(maxdt=max(eval(parse(text=dtkey)))),.(id=eval(parse(text=idkey)))]
  DT<-merge(DT,GDT,by.x=idkey,by.y = 'id')
  DT<-DT[eval(parse(text=dtkey))==maxdt]
  DT[,c('maxdt'):=NULL]
  return(DT)
}
 
onePerOneRecord<-function(DT,idkey,dtkey,decs=T){
  #reorder the data
  DT<-DT[order(eval(parse(text=idkey)),eval(parse(text=dtkey)))]
  #drop duplicated data and remain the latest data
  if(decs)
    DT[,'recordNum']<-DT[,.(recordNum=c(.N:1)),.(eval(parse(text=idkey)))]$recordNum else
      DT[,'recordNum']<-DT[,.(recordNum=c(1:.N)),.(eval(parse(text=idkey)))]$recordNum
  DT<-DT[recordNum==1]
  DT[,c('recordNum'):=NULL]
  return(DT)
}
#test data record type
dataUpdateStat<-function(idkey,dtkey,TBname){
    ch <- odbcConnect("impalaodbc", uid = "changlue.she", pwd = "9f671e71a33b3454b9593d4f32f6ff18")
    sqlTables(ch)
    sql    <- paste("select ",idkey,',',dtkey," from ",TBname," where ",idkey,' in (select distinct ',idkey,' from ',TBname, ' limit 10000)',sep='')
    dt     <- sqlQuery(ch,sql,as.is=T)%>%data.table
    dt     <- dt[,.(cot=.N),.(idkey=eval(parse(text=idkey)),dtkey=eval(parse(text=dtkey)))]
    NumPerDate<-dt$cot%>%mean%>%round(.,2)
    NumPer    <- dt[,.(cot=.N),.(idkey=idkey)]$cot%>%mean%>%round(.,2)
    dt     <- dt[order(idkey,dtkey)]
    dt     <- dt[,diff(cot),.(idkey)]         
    add    <- (as.numeric(dt$V1>0)%>%sum) 
    les    <- (as.numeric(dt$V1<0)%>%sum) 
     
    odbcClose(ch)
    print(paste('dtkey num per Person in per day',NumPerDate))
    print(paste('dtkey times per Person',NumPer))
    print(paste('add less rate',add/(add+les)))
    return(dt)
}
#save datas
saveOBJ<-function(OBJ,Parant_dir,saveType='.csv'){
  save_dir<-paste(Parant_dir,deparse(substitute(OBJ)),saveType,sep='')
  write.csv(OBJ,file =save_dir,row.names = F )
}
#load the saved datas
loadOBJ<-function(OBJ,Parant_dir,saveType='.csv'){
  save_dir<-paste(Parant_dir,deparse(substitute(OBJ)),saveType,sep='')
  OBJ<-fread(file =save_dir)
  return(OBJ)
}
#batch save data
batchSaveOBJ<-function(OBJ,Parant_dir,idkey='custorm_id',saveType='.csv'){
  save_dir <- paste(Parant_dir,deparse(substitute(OBJ)),saveType,sep='')
  tryCatch({
    old_OBJ  <- fread(file =save_dir)
    old_vars <- old_OBJ%>%names
    new_vars <- OBJ%>%names
    not_new_vars<-new_vars[!new_vars%in%old_vars]
    not_old_vars<-old_vars[!old_vars%in%new_vars]
    if(not_new_vars%>%length)
      old_OBJ[,not_new_vars]<-NA
    if(not_old_vars%>%length)
      OBJ[,not_old_vars]    <-NA
    keep<-!old_OBJ[,eval(parse(text=idkey))]%in%OBJ[,eval(parse(text=idkey))]
    if(keep[keep]%>%length)
      OBJ <- rbind(OBJ%>%data.frame,old_OBJ[keep]%>%data.frame)%>%data.table
  },error = function(e) print("initial saved"))
  write.csv(OBJ,file =save_dir,row.names = F )
}
 

#set feature names
setFeatNames<-function(nameL,loopOrder){
  featNames<-c()
  for(comp3 in nameL[[loopOrder[3]]]){
    for(comp2 in nameL[[loopOrder[2]]]){
      for(comp1 in nameL[[loopOrder[1]]]){
        comps<-c()
        comps[loopOrder[1]]<-comp1
        comps[loopOrder[2]]<-comp2
        comps[loopOrder[3]]<-comp3
        featNames<-c(featNames,paste(comps,collapse = '_'))
      }
    }
  }
  return(featNames)
}

miss_handle<-function(DT,vars,impute){
  for(var in vars){
    DT[eval(parse(text=var))%>%is.na,var]<-impute
  }
  return(DT)
}
 
source('~/project/LabelAutoComputeScripts/SHARE/CODE/featEngineering.R')