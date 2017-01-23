
#' reconnect the variable component
#'
#' reconnect the variable component
#' 
#' @param vars the variable's component

#' @export
#' @examples
#' 
#' cat_var(vars)
cat_var<-function(vars){
  temp<-vars[1]
  for(i in vars[-1]){
    temp<-paste(temp,i,sep ="_")
  }
  return(temp)
}

#' get the dates' info from variable name
#'
#' get the dates' info from variable name
#' 
#' @param date the num info
#' @param se to determine month,season 
#' @export
#' @examples
#' 
#' dates_handle(date)
dates_handle<-function(date,se=NA,type="ac"){
  if(length(date)==4)
    date<-c(paste(date[1],date[2],sep="")%>%as.numeric(),paste(date[3],date[4],sep="")%>%as.numeric())
  if(length(date)==3)
    date<-c(date[1],paste(date[2],date[3],sep="")%>%as.numeric())
  if(length(date)==2&date[2]%in%c(2,8))
    date<-paste(date[1],date[2],sep="")%>%as.numeric()
  if(!is.na(se)){
    dates=paste('m',seq(1,max(date),se),'m',seq(se,max(date),se),sep="")
  }else{
    if(type=='ac')
    dates<-min(date):max(date)else
      dates<-date
    dates<-paste('m',dates,sep='')
  }
  return(dates)
}
#' get those original variables' name from derived variable
#'
#' get those original variables' name from derived variable
#' 
#' @param dates dates' info
#' @param comp variable names' component
#' @param repla replace element
#' @param drop drop element
#' @param add add element
#' @param loc where to add
#' @export
#' @examples
#' 
#' get_col_list(useful,10)
get_origvar<-function(dates,comp,repla="m",drop="",add="",loc=1){
  comp<-unique(comp)
  comp[(loc+length(add)):(length(comp)+length(add))]<-comp[loc:length(comp)]
  comp[loc:(loc+length(add)-1)]<-add
  comp<-comp[!is.na(comp)]
  comp<-comp[!comp%in%c(drop,"")]
  ori<-c()
  for(i in dates){
    temp<-comp
    temp[temp%in%repla]<-i
    ori<-c(ori,cat_var(temp))
  }
  return(ori)
}

#' reconnect the variable component
#'
#' reconnect the variable component
#' 
#' @param vars the variable's component

#' @export
#' @examples
#' 
#' cat_var(vars)

match_oris<-function(oris,vars,l){
  comp<-unlist(str_extract_all(oris,"[a-z,A-Z,0-9]+"))
  loc<-1:length(comp)
  loc<-loc[-l]
  for(i in vars){
    compi<-unlist(str_extract_all(i,"[a-z,A-Z,0-9]+"))
    if(length(compi)!=length(comp))
      vars<-vars[vars!=i]else{
        for(j in loc){
        if(comp[j]!=compi[j])vars<-vars[vars!=i]
      }}
  }
  return(vars)
}


