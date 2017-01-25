#' fit the stable difference matrix
#'
#' fit the stable difference matrix
#' 
#' @param train.mt the Markov lineage
#' @param pred.mt the target vect
#' @param targ.vec lowest nums

#' @export
#' @examples
#' 
#' stable_difference(train.mt,pred.mt,targ.vec)
get_std_deri<-function(df,loop=2,nsmp=3000,threshold=0.2){
  df<-define_types(df,as.numeric)
  comple.rows<-which(complete.cases(df))
  y<-df[,1]
  df<-df[,-1]
  njs<-length(df)
  if(njs>12)
    njs<-12
  probs.vec<-c()
  iid<-0
  for(loo in 1:(loop*200)){
    threshold<-threshold*(1-1/(loop*200))
    rowj<-sample(comple.rows,njs)
    if(table(y[rowj])['0']>njs-1)
      next
    rowis<-sample(1:length(df[,1]),nsmp)
    probs<-get_sd_probs(rowj,df,y,njs,rowis)
    new<-0
    new<-ks(probs,y[rowis])
    print(new)
    if(new>threshold){
      probs<-get_sd_probs(rowj,df,y,njs)
      new2<-ks(probs,y)
      print(new2)
      if(new2>threshold/2){
        print(paste('small:',new,'large:',new2))
        probs.vec<-cbind(probs.vec,probs)
        iid<-iid+1
      }
    }
    if(iid>=loop)
      break
  }
  temp<-apply(probs.vec,1,function(s)mean(s,na.rm=T))
  return(temp)
}
#' transform the stable difference matrix
#'
#' transform the stable difference matrix
#' 
#' @param out.list the Markov lineage
#' @param kk the target vect
#' @param targ.vec  lowest nums

#' @export
#' @examples
#' 
#' get_sd_vec(out.list,kk,targ.vec)
get_sd_probs<-function(rowj,df,y,njs,rowis=NULL){
  if(is.null(rowis))
    rowis<-1:length(df[,1])
  probs<-c()
  for(rowi in rowis){
    if(length(which(!is.na(df[rowi,])))<njs/2|rowi%in%rowj){
      probs<-c(probs,NA)
      next
    }
    mat<-c()
    for(colk in 1:(njs-1)){
      for(coln in (colk+1):njs){
        true.value<-df[rowi,colk]
        pre<-df[rowj,coln]-df[rowi,coln]+df[rowj,colk]
        mat<-rbind(mat,c(true.value,pre))
      }
    }
    mat<-rbind(c(NA,y[rowj]),mat)
    mat<-data.frame(mat)
    fit=lm(X1~0+.,mat[-1,])
    pro<-sum(fit$coefficients*mat[1,-1],na.rm = T)
    probs<-c(probs,pro)
  }
  return(probs)
}