#' using a sequence backward selection way to select variables
#'
#' using a sequence backward selection way to select variables
#'
#' @param df the dataframe
#' @param useful the candidates variables
#' @param targ the label variable
#' @param a the threshold which determine whether the variable should be dropped
#' @param estimator the fit model to get some scoring
#' @export
#' @examples
#' SBS(p2p.model,useful,targ="def",a=0.003,estimator=xg_cv)
SBS<-function(df,useful,targ,a,estimator,scorer,nfolds=4,info=NULL,decreasing=F,aug=F,loops=2){
  if(!is.null(info)){
  info<-info[names(info)%in%useful]
  info<-info[order(info,decreasing = decreasing)]
  useful<-names(info)}
  temp<-estimator(df,useful,targ,scorer,nfolds)
  index<-1
  test.ks.serial<-temp["test"]
  train.ks.serial<-temp["train"]
  paste("the initial test score is",temp["test"])%>%print()
  paste("the initial train score is",temp["train"])%>%print()
  vars.keep.serial<-list()
  vars.keep.serial[[index]]<-useful
  old<-temp["test"]
  t.r.s<-length(useful)
  l.r.s<-t.r.s+1
  while(l.r.s>t.r.s&loops>0){
    loops<-loops-1
    l.r.s<-t.r.s
    for(i in useful){
      tryCatch({
        useful<-useful[useful!=i]
        temp<-estimator(df,useful,targ,scorer,nfolds)
        new<-temp["test"]
        print(paste("regardless of",i))
        print(c(new,temp["train"]))
        if(old-new>a){
          useful<-c(useful,i)
        }else{
          print(paste("because",i,"is useless,so drop it"))
          if(aug&old<new){
          old<-new}else if(!aug){
            old<-new
          }
          test.ks.serial<-c(test.ks.serial,temp["test"])
          train.ks.serial<-c(train.ks.serial,temp["train"])
          index<-index+1
          vars.keep.serial[[index]]<-useful
        }
      },error=function(e){
        useful<-c(useful,i)
        print("something wrong")
      }
      )
    }
    t.r.s<-length(useful)
    useful<-sample(useful,t.r.s,replace = F)
  }
  return(list(test=test.ks.serial,train=train.ks.serial,features=vars.keep.serial))
}
#' plot the test-train ks 
#'
#' plot the test-train ks to show how the variables' number influence ks
#'
#' @param test.s the test ks serial
#' @param train.s the train ks serial
#' @param vars.s the variables serials
#' @export
#' @examples
#' sbs_curve(test.s,train.s,vars.s)
sbs_curve<-function(fs.out){
  test.s<-fs.out[[1]]
  train.s<-fs.out[[2]]
  vars.s<-fs.out[[3]]
  id<-lapply(vars.s,length)%>%unlist()
  df<-data.frame(id,test.s,train.s)
  names(df)[2:3]<-c("test-ks","train-ks")
  ks.df<-melt(df,id.vars="id")
  print(ggplot(ks.df,aes(id,value,colour=variable))+geom_line()+xlab("variable numbers")+ylab("ks"))
}



#' using a sequence forward selection way to select variables
#'
#' using a sequence forward selection way to select variables
#'
#' @param df the dataframe
#' @param useful the candidates variables
#' @param candidate the candidates variables
#' @param targ the label variable
#' @param a the threshold which determine whether the variable should be dropped
#' @param estimator the fit model to get some scoring
#' @export
#' @examples
#' SFS(p2p,useful,candidate,targ="def",a=0.013,estimator=xg_cv)
SFS<-function(df,useful,candidate,targ,a,estimator){
  candidate<-sample(candidate,length(candidate),replace = F)
  temp<-estimator(df,useful,targ,scorer)
  index<-1
  test.ks.serial<-temp["test"]
  train.ks.serial<-temp["train"]
  vars.keep.serial<-list()
  vars.keep.serial[[index]]<-useful
  old<-temp["test"]
  t.r.s<-length(useful)
  l.r.s<-t.r.s+1
  while(l.r.s>t.r.s){
    l.r.s<-t.r.s
    for(i in candidate){
      tryCatch({
        useful<-c(useful,i)
        temp<-estimator(df,useful,targ,scorer)
        new<-temp["test"]
        print(paste("think about",i))
        print(new)
        if(new-old>a){
          old<-new
          candidate<-candidate[candidate!=i]
          print(paste("because",i,"is useful,so keep it"))
          test.ks.serial<-c(test.ks.serial,temp["test"])
          train.ks.serial<-c(train.ks.serial,temp["train"])
          index<-index+1
          vars.keep.serial[[index]]<-useful
          }else{
          useful<-useful[useful!=i]
        }
      },error=function(e){
        useful<-useful[useful!=i]
        print("something wrong")
      }
      )
    }
    t.r.s<-length(candidate)
    candidate<-sample(candidate,t.r.s,replace = F)
  }
  return(list(test=test.ks.serial,train=train.ks.serial,features=vars.keep.serial))
}



