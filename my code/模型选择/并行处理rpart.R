para_rpart<-function(df,var.list,cv.list,ks=ks,controls=controls.rpart){
  library(parallel)
  cl <- makeCluster(length(cv.list))
  ignore <- clusterEvalQ(cl, {library(rpart); NULL})
  results<-pmult_rpart(cl,df[,c(var.list,"def")],cv.list,ks,controls)
  detail<-sapply(results,rbind)
  temp<-apply(detail,1,mean)
  ks.all<-c()
  ks.all["test"]<-temp[1]
  ks.all["train"]<-temp[2]
  stopCluster(cl)
  return(list(detail,ks.all))
}

pmult_rpart <- function(cl,df,cv.list,ks,controls) {
  controls
  df
  ks
  cv.list
  a<-1:length(cv.list)
  mult <- function(s) {
    boost<-rpart(def~.,data=df[-cv.list[[s]],],control=controls)
    pred.te<-predict(boost,newdata=df[cv.list[[s]],],type = "prob")
    ks.test<-ks(pred.te[,2],df[cv.list[[s]],"def"],plots = FALSE)
    pred.tr<-predict(boost,newdata=df[-cv.list[[s]],],type = "prob")
    ks.train<-ks(pred.tr[,2],df[-cv.list[[s]],"def"],plots = FALSE)
    c(ks.test,ks.train)
  }
  parLapply(cl, a, mult)
}

