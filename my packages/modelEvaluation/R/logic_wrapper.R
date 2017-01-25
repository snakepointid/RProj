#' the wrappered logic regression model
#'
#' the wrappered logic regression model
#' 
#' @param df the dataframe
#' @param useful used features 
#' @param targ the label variable
#' @param nfolds the cross validation numbers
#' @export
#' @examples
#' 
#' para_logic(df =p2p.model,useful,targ="def",nfolds=4)
para_logic<-function(df,useful,targ,scorer,nfolds=4){
  set.seed(123)
  cv.list<-cv_balance(df,nfolds,targ=targ)
  cl <- makeCluster(nfolds)
  ignore <- clusterEvalQ(cl, {library(s.ME);library(C50); NULL})
  results<-pmult_logic(cl,df,useful,targ,scorer,cv.list)
  detail<-sapply(results,rbind)
  temp<-apply(detail,1,mean)
  names(temp)<-c("test","train")
  stopCluster(cl)
  return(temp)
}
#' the major component of para_logic
#'
#' the major component of para_logic
#' 
#' @param df the dataframe
#' @param useful used features 
#' @param targ the label variable
#' @export
#' @examples
#' 
#' pmult_logic(df =p2p.model,useful,targ="def")
pmult_logic <- function(cl,df,useful,targ,scorer,cv.list) {
  df
  useful
  targ
  cv.list
  a<-1:length(cv.list)
  formulas<-as.formula(paste(targ,"~."))
  mult <- function(s) {
    logic.fit<-glm(formulas,data=df[-cv.list[[s]],c(useful,targ)],family = binomial(link="logit"))
    
    prob<-predict(logic.fit,newdata=df[cv.list[[s]],useful],type = "response")
    ks.test<-scorer(prob,df[cv.list[[s]],targ])
    
    prob<-predict(logic.fit,newdata=df[-cv.list[[s]],useful],type = "response")
    ks.train<-scorer(prob,df[-cv.list[[s]],targ])
    c(ks.test,ks.train)
  }
  parLapply(cl, a, mult)
}


