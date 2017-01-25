#' doing discrete variables clustering
#'
#' doing discrete variables clustering
#' 
#' @param ldf the dataframe
#' @param k cluster numbers
#' @param m the feature numbers
#' @export
#' @examples
#' 
#' catvar_clust(ldf,m)
catvar_clust<-function(ldf,m){
  mat<-as.matrix(ldf)
  temp<-as.vector(mat)
  temp<-temp[!temp%in%c('',' ')]
  temp<-table(temp)
  features<-names(temp[order(temp,decreasing = T)][1:m])
  tdf<-apply(mat,1,function(s)as.numeric(features%in%s))
  return(t(tdf))
}
#' doing discrete variables clustering
#'
#' doing discrete variables clustering
#' 
#' @param ldf the dataframe
#' @param k cluster numbers
#' @param m the feature numbers
#' @export
#' @examples
#' 
#' discrete_vars_cluster(ldf,k,m)

df_cluster<-function(ldf,k){
  nobs<-length(ldf[,1])
  temp<-0
  for(i in 1:10){
    km<-kmeans(ldf[sample(1:nobs,round(nobs/10)),],centers=k)
    temp<-temp+km$centers
  }
  cen<-temp/10
  km<-kmeans(ldf,centers=cen)
  return(as.factor(km$cluster))
}
#' select best cluster paras
#'
#' select best cluster paras
#' 
#' @param ldf the dataframe
#' @param kk cluster numbers
#' @param mm the feature numbers
#' @param target the target variable
#' @export
#' @examples
#' 
#' find_cluster_bstpara(ldf,kk,mm,target)
find_cluster_bstpara<-function(ldf,kk,target,m=NULL){
  set.seed(1)
  bs.iv<-0
  bs.vec<-NA
  if(!is.null(m))
    ldf<-catvar_clust(ldf,m)
  for(i in kk){
      tryCatch({
        iv<-0
        temp<-df_cluster(ldf,i)
        iv<-get_IV(temp,target)
      },error=function(e){
        iv<-0
      }
      )
      if(iv>bs.iv){
        bs.iv<-iv
        bs.vec<-temp
        print(paste('cluster:',i,'the better iv is',iv))
    }
  }
  return(bs.vec)
}
