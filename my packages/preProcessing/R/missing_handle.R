
#' counting vector's most frequency value's rate
#' 
#' this function will compute the columns' most frequency value's rate
#'
#' @param df the dataframe
#' @param targ the label variable
#' @export
#' @examples
#' single_value_count(p2p,"def")

single_value_count<-function(df,targ){
  l<-length(df[,1])
  vars<-names(df)
  vars<-vars[vars!=targ]
  miss<-apply(df[,vars],2,function(s)table(s)%>%max()/l)
  return(miss[order(miss)])
}
#' handling those missing observation 
#' 
#' this function using a number to fill those missing observations
#' 
#'
#' @param df the dataframe
#' @param miss.value the number to fill those missing observations,if not set,those missing observartion will remain NA
#' @export
#' @examples
#' miss_handle(p2p,-1)
miss_handle<-function(df,targ,dis.val=NULL,num.val=NULL){
  df<-data.frame(df)
  for(i in names(df)){
    if(is.factor(df[,i])&i!=targ&!is.null(dis.val)){
      df[,i]<-as.character(df[,i])
      mp<-which(is.na(df[,i])|is.infinite(df[,i]))
      df[mp,i]<-dis.val
      df[,i]<-as.factor(df[,i])
    }else if(is.numeric(df[,i])&i!=targ&!is.null(num.val)){
        temp<-df[,i]
        mp<-which(is.na(df[,i])|is.infinite(df[,i]))
        df[mp,i]<-num.val
    }
  }
  if(length(df)==1)
    df<-df[,1]
  return(df)
}

#' define those missing observations
#' 
#' this function will change those filled missing observation into NA format
#'
#' @param df the dataframe
#' @param num.fill the numeric vector miss filled number
#' @param dis.fill the catogary vector miss filled value
#' @export
#' @examples
#' trans2miss(p2p,-1,NA)

trans_v2v<-function(df,value,change2){
  for(i in names(df)){
    df[,i]<-temp<-as.character(df[,i])
    temp[temp==value]<-change2
    df[,i]<-temp
  }
  return(df)
}


