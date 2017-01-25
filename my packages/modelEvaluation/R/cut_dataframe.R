#' cut the dataframe into training and testing parts
#'
#' This function cut the dataframe into training and testing parts
#' with balancing the previous ratio of the positive and negtive samples
#' 
#' @param df the dataframe
#' @param prop the proportion of the tesing datas
#' @param targ the label variable
#' @export
#' @examples
#' test.position = balance_cut(p2p,0.25,"def")
#' traindf<-df[-test.position,]
#' testdf<-df[test.position,]

balance_cut<-function(df,prop,targ){
  g.num<-which(df[,targ]==0)
  b.num<-which(df[,targ]==1)
  g.l<-length(g.num)*prop
  b.l<-length(b.num)*prop
  g.sam<-sample(g.num,g.l,replace=F)
  b.sam<-sample(b.num,b.l,replace=F)
  samples<-c(g.sam,b.sam)
  for(i in 1:5){
    samples<-sample(samples,length(samples),replace = F)
  }
  return(samples)
}

#' get a cross validation data pairs
#'
#' This function will  get a cross validation data pairs
#'
#' @param df the dataframe
#' @param folds the numbers of pairs
#' @param targ the label variable
#' @export
#' @examples
#' cv.list<-cv_balance(p2p,4,"def")
#' traindf<-df[-cv.list[[1]],]
#' testdf<-df[cv.list[[1]],]
cv_balance<-function(df,folds,targ){
  g_b<-as.character(df[,targ])
  cv.list<-list()
  remain.g<-which(g_b=="0")
  remain.b<-which(g_b=="1")
  
  g.ns<-length(remain.g)
  g.ns<-g.ns/folds
  b.ns<-length(remain.b)
  b.ns<-b.ns/folds
  for(i in 1:folds){
    samp.g<-sample(remain.g,g.ns,replace = F)
    samp.b<-sample(remain.b,b.ns,replace = F)
    all<-c(samp.b,samp.g)
    cv.list[[i]]<-sample(all,length(all),replace = F)
    remain.g<-remain.g[!remain.g%in%samp.g]
    remain.b<-remain.b[!remain.b%in%samp.b]
  }
  return(cv.list)
}

