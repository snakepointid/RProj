#' get missing or outlier information
#'
#' get missing or outlier information
#'
#' @param df the discreting dataframe by best seperation
#' @param vars the choosing variables
#' @export
#' @examples
#' extra_out_miss(p2p,con.vars,"con.miss",out=F)
extra_out_miss<-function(df,vars){
  if(outers_[1]){
    temp<-c()
    for(i in vars){
      op<-find_outlier(df[,i],dfforouters[,i])
      temp<-c(temp,op)
    }
  }else{
    temp<-apply(df[,vars],2,function(s)which(is.na(s)))
    temp<-unlist(temp)
  }
  temp<-c(temp,1:length(df[,1]))
  temp<-table(temp)
  temp<-temp-1
  vec<-as.numeric(temp)
  return(vec)
}
#' get new vec information
#'
#' get new vec information
#'
#' @param df the discreting dataframe by best seperation
#' @param useful the choosing variables
#' @param targ the target variable
#' @param score score estimater
#' @export
#' @examples
#' get_miss_out_iv(df,useful,targ,score,nfold=1)
get_miss_out_iv<-function(df,useful,targ,score,nfold=1){
  new.vec<-extra_out_miss(df,useful)
  iv<-abs(score(new.vec,df[,targ]))
  temp<-c(iv,iv)
  names(temp)<-c("test","train")
  return(temp)
}
#' find outlier info
#'
#' find outlier info
#'
#' @param vec the choosen vector
#' @export
#' @examples
#' find_outlier(p2p[,1])
find_outlier<-function(vec,vecforouters){
  if(is.numeric(vec)){
    u<-mean(vecforouters,na.rm=T)
    std<-sd(vecforouters,na.rm=T)
    higher<-which(vec>u+3*std)
    lower<-which(vec<u-3*std)
    if(outers_[3]=='H')
      higher<-c()
    if(outers_[3]=='L')
      lower<-c()
    return(c(higher,lower))
  }else if(is.factor(vec)){
    vec<-as.character(vec)
    vecforouters<-as.character(vecforouters)
    if(outers_[2])
      vecforouters[is.na(vecforouters)]<-'missing'
    temp<-table(vecforouters)%>%prop.table()
    outer<-temp[temp<0.05]
    outer<-names(outer)
    outer<-which(vec%in%outer)
    return(outer)
  }
}

