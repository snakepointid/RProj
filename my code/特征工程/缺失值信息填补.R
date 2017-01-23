
change_col<-function(target,predictor){
  temp<-table(target)
  glo.odd<-temp[2]/temp[1]
  if(is.factor(predictor)){
    temp<-table(target,predictor)
    odds<-temp[2,]/temp[1,]
    value<-names(odds)
    bad.h<-value[which(odds>glo.odd)]
    good.h<-value[which(odds<glo.odd)]
    newpredictor<-as.character(predictor)
    bp<-which(newpredictor%in%bad.h)
    newpredictor[bp]<-1
    newpredictor[-bp]<-0
  }else{
    temp.ks<-ks(predictor,target)
    sep.point<-names(temp.ks)%>%as.numeric()
    mp<-which(predictor>=sep.point)
    newpredictor<-predictor
    if(temp.ks>0){
      newpredictor[mp]<-"h.bad"
      newpredictor[-mp]<-"h.good"
    }else{
      newpredictor[mp]<-"h.good"
      newpredictor[-mp]<-"h.bad"
    }
    }
  return(newpredictor)
}

submodel<-function(df,targ,sub.targ,use.predict,iv.threshold){
  mp<-which(!is.na(df[,sub.targ]))
  temp<-names(df)
  predictors<-temp[!temp%in%c(targ,use.predict)]

  y.train<-change_col(df[mp,targ],df[mp,sub.targ])
  gp<-which(y.train=="h.good")
  y.train[gp]<-0
  y.train[-gp]<-1
  y.train<-as.numeric(y.train)
  x.train<-df[mp,predictors]
  x.predictors<-df[-mp,predictors]
  targ.predictors<-df[-mp,targ]

  miss<-missing_count(x.train)
  wm<-which(miss<0.7)
  useful<-names(miss)[wm]
  miss<-missing_count(x.predictors[,useful])
  wm<-which(miss<0.7)
  useful<-names(miss)[wm]

  miss<-single_value_count(x.train[,useful],"def")
  wm<-which(miss<0.7)
  useful<-names(miss)[wm]

  lowest<-50

  for(i in useful){
    tryCatch({
      iv<-get_IV(x.train[,i],y.train,lowest)
    },error=function(e){
      iv<-1
    }
    )
    if(iv>iv.threshold){
      useful<-useful[useful!=i]
    }
  }
  return(list(y.train,x.train[,useful],x.predictors[,useful],targ.predictors))
}

model_construct<-function(sub.model,test=T,nround){
  
  df<-data.frame(sub.model[[1]],sub.model[[2]])
  df<- miss_handle(df,-1)
  targ<-names(df)[1]
  useful<-names(df)[-1]
  if(test){
    temp<-xg_cv(df,useful,targ,show=T)
  }else{
  bst<-xg_model(df,useful,targ,nround)
  }
  return(bst)
}

model_predict<-function(bst,sub.model){
  
    train.mat <- sparse.model.matrix(~., data = miss_handle(sub.model[[3]],-1))
    prob<-predict(bst,newdata=train.mat)
    temp.ks<-ks(prob,sub.model[[4]])


  if(temp.ks>0.1){
    print(paste("it works,ks:",temp.ks))
    new.vect<-change_col(sub.model[[4]],prob)
    return(new.vect)
  }else{
    return("missing")
  }
}

