para_C50<-function(df,var.list,cv.list,ks=ks,controls=controls.C50){
  library(parallel)
  cl <- makeCluster(length(cv.list))
  ignore <- clusterEvalQ(cl, {library(C50); NULL})
  results<-pmult_C50(cl,df[,c(var.list,"def")],cv.list,ks,controls)
  detail<-sapply(results,rbind)
  temp<-apply(detail,1,mean)
  ks.all<-c()
  ks.all["test"]<-temp[1]
  ks.all["train"]<-temp[2]
  stopCluster(cl)
  return(list(detail,ks.all))
}

pmult_C50 <- function(cl,df,cv.list,ks,controls=controls.C50) {
  controls
  df
  ks
  cv.list
  a<-1:length(cv.list)
  mult <- function(s) {
    boost<-C5.0(def~.,data=df[-cv.list[[s]],],trials=1,
                control=controls)
    pred.te<-predict(boost,newdata=df[cv.list[[s]],],type = "prob")
    ks.test<-ks(pred.te[,2],df[cv.list[[s]],"def"],plots = FALSE)
    pred.tr<-predict(boost,newdata=df[-cv.list[[s]],],type = "prob")
    ks.train<-ks(pred.tr[,2],df[-cv.list[[s]],"def"],plots = FALSE)
    c(ks.test,ks.train)
    }
  parLapply(cl, a, mult)
}


c50_estimate<-function(df,useful,scorer,cut.p){
  set.seed(123)
  tp<-balance_cut(df,cut.p,"def")
  train<-df[-tp,c(useful,"def")]
  test<-df[tp,c(useful,"def")]
  fit<-C5.0(def~.,train,control =c50.paras)
  probs<-predict(fit,train,type='prob')
  train.s<-scorer(probs,train[,"def"])
  
  probs<-predict(fit,test,type='prob')
  test.s<-scorer(probs,test[,"def"])
  return(c(train.s,test.s))
}