#' the wrappered xgboost cross validation model
#'
#' the wrappered xgboost cross validation model
#' 
#' @param df the dataframe
#' @param useful used features 
#' @param targ the label variable
#' @export
#' @examples
#' trees<-200
#' xg_cv(df =p2p.model,useful,targ="def")
xg_cv<-function(df,useful,targ,scorer=scorer,nfolds=4,para=best.para,show=F){
  evalerror <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    err <- scorer(preds,labels)
    return(list(metric = "scoring", value = err))
  }
  if(!is.function(scorer)){
    err<-scorer
    cerr<-NULL
  }else{
    err<-NULL
    cerr<-evalerror
  }
  
  
  train.mat <- sparse.model.matrix(~.,data=df[,useful])
  train.l<-df[,targ]%>%as.character()%>%as.numeric()
  dtrain <- xgb.DMatrix(data =train.mat,label = train.l,missing = m.v)
  
  set.seed(123)
  bst <- xgb.cv(params=para, data=dtrain,nround =mean(trees)+30,verbose = show,nfold=nfolds,feval=cerr,metrics =err)
  train.vec<-bst[[1]]
  test.vec<-bst[[3]]
  wp<-test.vec%>%which.max()
  test.ks<-test.vec[wp]
  train.ks<-train.vec[wp]
  trees<<-c(trees,wp)
  temp<-c(test.ks,train.ks)
  names(temp)<-c("test","train")
  return(temp)
}


#' the wrappered xgboost model
#'
#' the wrappered xgboost model
#' 
#' @param df the dataframe
#' @param useful used features 
#' @param targ the label variable
#' @param nround the trees number
#' @param para the trees control parameter
#' @export
#' @examples
#' trees<-200
#' best.para <- list(max.depth =2, eta = 0.1,objective="binary:logistic")
#' xg_model(df =p2p.model,useful,targ="def")
xg_train<-function(df,useful,targ,nround=trees,para=best.para,scorer=ks){
  useful<-useful[useful!=targ]
  train.mat <-  sparse.model.matrix(~.,data=df[,useful])
  train.l<-df[,targ]%>%as.character()%>%as.numeric()
  dtrain <- xgb.DMatrix(data =train.mat,label = train.l,missing = m.v)
  
  set.seed(123)
  bst <- xgb.train(params=para, data=dtrain,nround =nround,eval_metric =scorer)
  return(bst)
}

#' the wrappered xgboost predict 
#'
#' the wrappered xgboost predict 
#' 
#' @param df the dataframe
#' @param useful used features 
#' @param bst the trained xgboost model
#' @export
#' @examples
#' xg_predict(test.df,useful,bst)
xg_predict<-function(df,useful,bst){
  train.mat <-  sparse.model.matrix(~.,data=df[,useful])
  dtrain <- xgb.DMatrix(data =train.mat,missing = m.v)
  set.seed(123)
  prob <- predict(bst,newdata=dtrain)
  return(prob)
}