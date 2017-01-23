#' get the base aggregation values
#'
#' get the base aggregation values
#' 
#' @param idx.vec
#' @param value.vec
#' @param var.name

#' @export
#' @examples
#' 
#' group_base(idx.vec,value.vec,'log1')
group_base<-function(idx.vec,value.vec,var.name){
  idx.vec<-idx.vec%>%as.character() 
  ob.idx<-unique(idx.vec)
  #count the object appear nums
  count.vec<-table(idx.vec)[ob.idx]
  #count the onject diff cate appear nums
  cate.count.mat<-table(idx.vec,value.vec)[ob.idx,]
  #count the most frequ cate
  cate.names<-names(cate.count.mat[1,])
  most.fre.cate<-cate.names[apply(cate.count.mat,1,which.max)]
  most.fre.cate.num<-apply(cate.count.mat,1,max)
  base.df<-data.frame(ob.idx,count.vec,most.fre.cate,most.fre.cate.num,cate.count.mat,stringsAsFactors = F)
  names(base.df)<-c('Idx',paste(var.name,c('count','most_cat','most_cat_num',cate.names),sep='_'))
  return(list('base'=base.df,'ccm'=cate.count.mat))
}

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
  #order object by time
  fir.cat<-c()
  las.cat<-c()
  fir3.cat<-c()
  las3.cat<-c()
  cont.fre.cat<-c()
  cont.fre.cat.num<-c()
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
    #first category
    fir.cat<-c(fir.cat,value[1])
    #last category
    las.cat<-c(las.cat,value[l.m])
    #first and last three most fre cate
    if(l.m>3){
      temp<-table(value[1:3])
      na.tmp<-names(temp)
      fir3.cat<-c(fir3.cat,na.tmp[which.max(temp)])
      temp<-table(value[(l.m-2):l.m])
      na.tmp<-names(temp)
      las3.cat<-c(las3.cat,na.tmp[which.max(temp)])
    }else{
      temp<-table(value)
      na.tmp<-names(temp)
      mscat<-na.tmp[which.max(temp)]
      fir3.cat<-c(fir3.cat,mscat)
      las3.cat<-c(las3.cat,mscat)
    }
    #most continue cat and nums
    l.i<-value[1]
    cot<-0
    ms.cot<-0
    for(i in value){
      if(i==l.i)
        cot<-cot+1 else
          cot<-1
        if(cot>ms.cot)
        {
          ms.cot<-cot
          ms.cat<-i}
        l.i<-i
    }
    cont.fre.cat<-c(cont.fre.cat,ms.cat)
    cont.fre.cat.num<-c(cont.fre.cat.num,ms.cot)
  }
  cat_time.df<-data.frame(ob.idx,fir.cat,las.cat,fir3.cat,las3.cat,cont.fre.cat,cont.fre.cat.num,stringsAsFactors = F)
  names(cat_time.df)<-c('Idx',paste(var.name,c('fir.cat','las.cat','fir3.cat',
                                               'las3.cat','cont.fre.cat','cont.fre.cat.num'),sep='_'))
  markv.mat<-cbind(idx.ord,value.1,value.lag)
  return(list('catime'=cat_time.df,'markv'=markv.mat))
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
  ave.time.gap<-c()
  all.time.gap<-c()
  max.time.gap<-c()
  ##
  most.fre.week<-c()
  least.fre.week<-c()
  most.fre.time.num<-c()
  #time
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
    ####time
    #avergy and max time gap
    ave.time.gap<-c(ave.time.gap,mean(diff(time)))
    all.time.gap<-c(all.time.gap,time[l.m]-time[1])
    max.time.gap<-c(max.time.gap,max(diff(time)))
    #most fre and least days
    temp<-table(week)
    mfd<-names(temp)[which.max(temp)]
    lfd<-names(temp)[which.min(temp)]
    most.fre.week<-c(most.fre.week,mfd)
    least.fre.week<-c(least.fre.week,lfd)
    temp<-table(time)
    most.fre.time.num<-c(most.fre.time.num,max(temp))
  }
  time.df<-data.frame(ob.idx,ave.time.gap,all.time.gap,max.time.gap,most.fre.week,least.fre.week,
                      most.fre.time.num,stringsAsFactors = F)
  names(time.df)<-c('Idx',paste(var.name,c('ave_timegap','all_timegap','max_timegap',
                                           'mostfre_week','lstfre_week','fre.time.num'),sep='_'))
  markv.mat<-cbind(idx.ord,value.1,value.lag)
  return(list('time'=time.df,'markv'=markv.mat))
}

