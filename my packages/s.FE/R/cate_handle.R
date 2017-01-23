#' interchanged switch the odds in different subsamples
#'
#' This function will interchanged switch the odds in different subsamples
#'
#' @param df the discreting dataframe by best seperation
#' @param targ the label variable
#' @param fold the subsample numbers
#' @export
#' @examples
#' value2interchanged_odds(alldata,'target',4)
value2interchanged_odds<-function(df,targ,fold){
  set.seed(123)
  y<-as.numeric(as.character(df[,targ]))
  targ.mp<-which(y%in%c(1,0))
  vali.mp<-which(!y%in%c(1,0))
  a.l<-length(targ.mp)
  f.l<-round(a.l/fold)
#   vali.a.l<-length(vali.mp)
#   vali.f.l<-round(vali.a.l/fold)
  for(i in names(df)){
    if(!is.factor(df[,i])|i==targ)
      next
    print(paste('handle',i))
    df[,i]<-as.character(df[,i])
    df[is.na(df[,i]),i]<-'missing##'
    feature<-unique(df[,i])
    sub1.mp<-sample(targ.mp,f.l)
    retain.mp<-targ.mp[!targ.mp%in%sub1.mp]
#     vali.sub1.mp<-sample(vali.mp,vali.f.l)
#     vali.retain.mp<-vali.mp[!vali.mp%in%vali.sub1.mp]
    subl.odds<-prop.table(table(df[sub1.mp,i],y[sub1.mp]),1)[,'1']
    all.odds<-subl.odds[feature]
    for(j in 2:fold){
      if(j<fold){
        sub.mp<-sample(retain.mp,f.l)
        #vali.sub.mp<-sample(vali.retain.mp,vali.f.l)
        retain.mp<-retain.mp[!retain.mp%in%sub.mp]
        #vali.retain.mp<-vali.retain.mp[!vali.retain.mp%in%vali.sub.mp]
      }else{
        sub.mp<-retain.mp
       # vali.sub.mp<-vali.retain.mp
      }
      sub.odds<-prop.table(table(df[sub.mp,i],y[sub.mp]),1)[,'1']
      df[sub.mp,i]<-subl.odds[df[sub.mp,i]]
      #df[vali.sub.mp,i]<-subl.odds[df[vali.sub.mp,i]]
      subl.odds<-sub.odds
      all.odds<-rbind(all.odds,subl.odds[feature])
    }
    df[sub1.mp,i]<-subl.odds[df[sub1.mp,i]]
    vali.odds<-apply(all.odds,2,function(s)mean(s,na.rm=T))
    df[vali.mp,i]<-vali.odds[df[vali.mp,i]]
    df[,i]<-as.numeric(df[,i])
  }
  return(df)
}
#' change discrete variable to bad odds
#'
#' This function will change discrete variable to bad odds
#'
#' @param df the dataframe  
#' @param targ the label variable
#' @param min.rat the minimal sample 
#' @export
#' @examples
#' one_hot_transform(alldata,'target',0.1)
one_hot_transform<-function(df,targ,min.rat=0.05){
  for(i in names(df)){
    if(is.factor(df[,i])&i!=targ){
      vec<-as.character(df[,i])
      vec[is.na(vec)]<-'missing'
      all.num<-prop.table(table(vec))
      values<-names(all.num)[all.num>min.rat]
      if(length(values)<1)
        values<-names(all.num[order(all.num,decreasing = T)])[1]
      for(j in values){
        var.na<-paste(i,j,sep='_')
        df[,var.na]<-as.numeric(vec==j)
      }
    }
  }
  return(df)
}
#' change discrete variable to bad odds
#'
#' This function will change discrete variable to bad odds
#'
#' @param df the discreting dataframe by best seperation
#' @param vars the vars need to change
#' @param targ the label variable
#' @export
#' @examples
#'value2odds(df,vars,targ,lowest=50)
value2odds<-function(df,vars,targ,lowest=50){
  for(i in vars){
    df[,i]<-as.character(df[,i])
    df[is.na(df[,i]),i]<-'missing##'
    print(paste('handle',i))
    temp1<-apply(table(df[,i],df[,targ]),1,sum)
    temp1<-names(temp1)[temp1>lowest]
    temp<-prop.table(table(df[,i],df[,targ]),1)[,'1']
    temp<-temp[temp1]
    df[,i]<-temp[as.character(df[,i])]
    mp<-which(is.na(df[,i]))
    df[mp,i]<-prop.table(table(df[mp,targ]))['1']
  }
  return(df)
}

#' interchanged switch average y 
#'
#' This function will interchanged switch average y 
#'
#' @param df the discreting dataframe by best seperation
#' @param targ the label variable
#' @param fold the subsample numbers
#' @export
#' @examples
#' value2interchanged_num(alldata,'target',4,median)


value2interchanged_num<-function(df,targ,fold,avgfunc){
  set.seed(123)
  y<-as.numeric(as.character(df[,targ]))
  targ.mp<-which(!is.na(y))
  vali.mp<-which(is.na(y))
  a.l<-length(targ.mp)
  f.l<-round(a.l/fold)

  for(i in names(df)){
    if(!is.factor(df[,i])|i==targ)
      next
    print(paste('handle',i))
    df[,i]<-as.character(df[,i])
    df[is.na(df[,i]),i]<-'missing##'
    feature<-unique(df[,i])
    sub1.mp<-sample(targ.mp,f.l)
    retain.mp<-targ.mp[!targ.mp%in%sub1.mp]
    gdf<-data.frame(id=df[sub1.mp,i],value=y[sub1.mp]) 
    gdf<-group_by(gdf,id)
    gdf<-summarise(gdf,mv=avgfunc(value,na.rm=T))
    subl.odds<-gdf$mv
    names(subl.odds)<-gdf$id
    all.odds<-subl.odds[feature]
    for(j in 2:fold){
      if(j<fold){
        sub.mp<-sample(retain.mp,f.l)
        
        retain.mp<-retain.mp[!retain.mp%in%sub.mp]
        
      }else{
        sub.mp<-retain.mp
       
      }
      gdf<-data.frame(id=df[sub.mp,i],value=y[sub.mp]) 
      gdf<-group_by(gdf,id)
      gdf<-summarise(gdf,mv=avgfunc(value,na.rm=T))
      sub.odds<-gdf$mv
      names(sub.odds)<-gdf$id
      df[sub.mp,i]<-subl.odds[df[sub.mp,i]]
       
      subl.odds<-sub.odds
      all.odds<-rbind(all.odds,subl.odds[feature])
    }
    df[sub1.mp,i]<-subl.odds[df[sub1.mp,i]]
    vali.odds<-apply(all.odds,2,function(s)mean(s,na.rm=T))
    names(vali.odds)<-feature
    df[vali.mp,i]<-vali.odds[df[vali.mp,i]]
    df[,i]<-as.numeric(df[,i])
  }
  return(df)
}