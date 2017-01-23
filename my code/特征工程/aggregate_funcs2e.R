#' get the advanced aggregation values with time and status
#'
#' get the advanced aggregation values with time and status
#' 
#' @param idx.vec s
#' @param value.vec s
#' @param time.vec s
#' @param targ.vec s
#' @param var.name s

#' @export
#' @examples
#' 
#' group_cat_time(idx.vec,value.vec,time.vec,targ.vec,var.name)
group_cat_time<-function(idx.vec,value.vec,time.vec,targ.vec,var.name){
  mp<-which(idx.vec%in%names(targ.vec))
  idx.vec<-idx.vec[mp]%>%as.character()
  time.vec<-as.numeric(time.vec[mp])
  value.vec<-value.vec[mp]
  #######
  ob.idx<-unique(idx.vec)
  value.1<-c()
  value.lag<-c()
  idx.ord<-c()
  for(idx in ob.idx){
    print(idx)
    #order the observe by group time
    mp<-which(idx.vec==idx)
    l.m<-length(mp)
    value<-value.vec[mp]
    time<-time.vec[mp]
    ord<-order(time,decreasing = F)
    value<-value[ord]
    #markve lining
    value.1<-c(value.1,value)
    value.lag<-c(value.lag,value[-1],'end')
    idx.ord<-c(idx.ord,idx.vec[mp])
    }
     markv.mat<-cbind(idx.ord,value.1,value.lag)
  return(list('markv'=markv.mat))
}
#' get the advanced aggregation values with time
#'
#' get the advanced aggregation values with time
#' 
#' @param idx.vec s
#' @param time.vec s
#' @param targ.vec s
#' @param var.name s

#' @export
#' @examples
#' 
#' group_time(idx.vec,time.ve,targ.vec,'time_log')
group_time<-function(idx.vec,time.ve,targ.vec,var.name){
  mp<-which(idx.vec%in%names(targ.vec))
  idx.vec<-idx.vec[mp]%>%as.character()
  week.vec<-weekdays(time.ve[mp])
  time.vec<-as.numeric(time.ve[mp])
  ######
  ob.idx<-unique(idx.vec)
  value.1<-c()
  value.lag<-c()
  idx.ord<-c()
  for(idx in ob.idx){
    print(idx)
    #order the observe by group time
    mp<-which(idx.vec==idx)
    l.m<-length(mp)
    time<-time.vec[mp]
    week<-week.vec[mp]
    ord<-order(time,decreasing = F)
    time<-time[ord]
    week<-week[ord]
    #markve lining
    value.1<-c(value.1,week)
    value.lag<-c(value.lag,week[-1],'end')
    idx.ord<-c(idx.ord,idx.vec[mp])
    }
    markv.mat<-cbind(idx.ord,value.1,value.lag)
  return(list('time'=time.df,'markv'=markv.mat))
}

