
stable_difference<-function(train.mt,pred.mt,targ.vec){
  train.mat<-as.matrix(train.mt[,-1])
  rownames(train.mat)<-train.mt[,1]
  pred.mat<-as.matrix(pred.mt[,-1])
  rownames(pred.mat)<-pred.mt[,1]
  out.list<-list()
  for(i in 1:length(pred.mat[,1])){
    idx<-rownames(pred.mat)[i]
    temp.mat<-train.mat[rownames(train.mat)!=idx,]
    vec<-apply(temp.mat,1,function(s)sd(s-pred.mat[i,]))
    vec[vec==0]<-min(vec[vec!=0])*0.01
    vec<-vec[order(vec,decreasing = F)]
    vec<-1/vec
    names(vec)<-targ.vec[names(vec)]
    vec[names(vec)=='0']<-vec[names(vec)=='0']*(-1)
    out.list[[idx]]<-vec
  }
  return(list(out.list,lapply(out.list,function(s){as.numeric(names(s))})))
}

get_sd_vec<-function(out.list,kk,targ.vec){
  old <- 0
  best.vec <- NA
  for (k in 1:kk) {
    out <- lapply(out.list, function(s) {
      mean(s[1:k])
    }) %>% unlist()
    key <- names(out)[names(out) %in% names(targ.vec)]
    new <- ks(out[key], targ.vec[key])
    if (new > old) {
      old <- new
      paste("k is", k, "ks is", new) %>% print()
      best.vec <- out
    }
  }
  return(best.vec)
}