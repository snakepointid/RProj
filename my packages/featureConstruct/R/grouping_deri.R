#' get the base aggregation values
#'
#' get the base aggregation values
#' 
#' @param idx.vec s
#' @param value.vec s
#' @param var.name s

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
  cate.count.mat<-data.frame(ob.idx,table(idx.vec,value.vec)[ob.idx,],stringsAsFactors = F)
  #count the most frequ cate
  return(cate.count.mat)
}

#' get the advanced aggregation values with time and status
#'
#' get the advanced aggregation values with time and status
#' 
#' @param idx.vec s
#' @param value.vec s
#' @param time.vec s
#' @param targ.vec s

#' @export
#' @examples
#' 
#' group_cat_time(idx.vec,value.vec,time.vec,targ.vec)
group_cat_time<-function(idx.vec,value.vec,time.vec,targ.vec){
  mp<-which(idx.vec%in%names(targ.vec))
  idx.vec<-idx.vec[mp]%>%as.character()
  time.vec<-as.numeric(time.vec[mp])
  value.vec<-value.vec[mp]
  #######################
  temp<-data.frame(idx.vec,time.vec,value.vec,stringsAsFactors = F)
  ##############################################################
  temp<-temp[order(temp$idx.vec,temp$time.vec),]
  ##############################################################
  temp <- group_by(temp,idx.vec)
  ##############################################################
  idx.id<-data.frame(idx.vec=temp$idx.vec,id.vec=1:length(temp$idx.vec),stringsAsFactors = F)
  idx.id <- group_by(idx.id,idx.vec)
  id.vec <- summarise(idx.id ,fis=first(id.vec))
  add.vec<-temp$value.vec%>%as.character()
  add.vec[id.vec$fis]<-'end'
  temp$add.vec<-c(add.vec[-1],'end')
  ##############################################################
  markov<-data.frame(Idx=temp$idx.vec,value=temp$value.vec,lag=temp$add.vec,stringsAsFactors = F)
  return(as.matrix(markov))
}

#' get the advanced aggregation values with time
#'
#' get the advanced aggregation values with time
#' 
#' @param idx.vec s
#' @param time.vec s
#' @param targ.vec s


#' @export
#' @examples
#' 
#' group_time(idx.vec,time.ve,targ.vec)
group_time<-function(idx.vec,time.vec,targ.vec){
  mp<-which(idx.vec%in%names(targ.vec))
  idx.vec<-idx.vec[mp]%>%as.character()
  value.vec<-weekdays(time.vec[mp])
  time.vec<-as.numeric(time.vec[mp])
  ##################################################
  temp<-data.frame(idx.vec,time.vec,value.vec,stringsAsFactors = F)
  ##############################################################
  temp<-temp[order(temp$idx.vec,temp$time.vec),]
  ##############################################################
  temp <- group_by(temp,idx.vec)
  ##############################################################
  idx.id<-data.frame(idx.vec=temp$idx.vec,id.vec=1:length(temp$idx.vec),stringsAsFactors = F)
  idx.id <- group_by(idx.id,idx.vec)
  id.vec <- summarise(idx.id ,fis=first(id.vec))
  add.vec<-temp$value.vec%>%as.character()
  add.vec[id.vec$fis]<-'end'
  temp$add.vec<-c(add.vec[-1],'end')
  ##############################################################
  markov<-data.frame(Idx=temp$idx.vec,value=temp$value.vec,lag=temp$add.vec,stringsAsFactors = F)
  return(as.matrix(markov))
}


