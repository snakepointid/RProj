#' get numeric variables' statistics index
#'
#' This function will get numeric variables' statistics index
#'
#' @param df the discreting dataframe by best seperation
#' @param key.vec the vars need to change
#' @param varname the label variable
#' @param reordervar whether reorder the dataframe by media 
#' @export
#' @examples
#' get_num_statis(alldata,alldata[,'Idx'],'varname',reordervar=F)
get_num_statis<-function(df,key.vec,varname,targ.vec=NULL){
  if(length(key.vec)!=length(unique(key.vec)))
    stop('key must be unique')
  tdf<-define_types(df,as.numeric)
  if(!is.null(targ.vec)){
    mp<-which(as.character(targ.vec)=='1')
    ut<-apply(tdf[mp,],2,function(s)median(s,na.rm = T))
    tdf<-tdf[,names(tdf)[order(ut)]]
    }
  key.vec<-as.character(key.vec)
  q1<-apply(tdf,1,function(s)quantile(s,0.25,na.rm = T))
  q2<-apply(tdf,1,function(s)quantile(s,0.5,na.rm = T))
  q3<-apply(tdf,1,function(s)quantile(s,0.75,na.rm = T))
  sum<-apply(tdf,1,function(s)sum(s,na.rm=T))
  min<-apply(tdf,1,function(s)min(s,na.rm=T))
  max<-apply(tdf,1,function(s)max(s,na.rm=T))
  ssd<-apply(tdf,1,function(s)sd(s,na.rm=T))
  inc_n1<-apply(tdf,1,function(s)get_inc(s,type='n1'))
  inc_n2<-apply(tdf,1,function(s)get_inc(s,type='n2'))
  inc_log<-apply(tdf,1,function(s)get_inc(s,type='log'))
  inc_log2<-apply(tdf,1,function(s)get_inc(s,type='log2'))
  w.max<-apply(cbind(-Inf,tdf),1,which.max)
  w.min<-apply(cbind(Inf,tdf),1,which.min)
  rdf<-data.frame(q1,q2,q3,sum,min,max,ssd,inc_n1,inc_n2,inc_log,inc_log2,w.max,w.min,stringsAsFactors = F)
  names(rdf)<-paste(varname,names(rdf),sep="_")
  rdf<-data.frame(Idx=key.vec,rdf,stringsAsFactors = F)
  paste('have finished ',varname,"'s statistics",sep='')
  return(rdf)
}
#' get incline of a vector
#'
#' This function will get incline of a vector
#'
#' @param value the discreting dataframe by best seperation
#' @param type the vars need to change
#' @export
#' @examples
#' get_inc(value,type='n1')
get_inc<-function(value,type='n1'){
  if(length(table(value))!=0){
    b<-1:length(value)
    if(type=='n1')
      inc<-lm(value~b)$coef[2]else if(type=='n2')
        inc<-lm(value~I(b^2)+b)$coef[2]else if(type=='log')
          inc<-lm(log(value+1)~b)$coef[2]else
            inc<-lm(log(value+1)~I(b^2)+b)$coef[2]
          return(inc)
  }else
    return(NA)
}



