
#' find the strange sign in the dataframe
#'
#' find the strange sign in the dataframe
#'
#' @param df the dataframe need to be exploit
#' @param n the numbers of signs to show
#' @export
#' @examples
#' find_strange_sign(p2p,100)


find_strange_sign<-function(df,n){
  sign<-c()
  for(i in names(df)){
    vec<-df[,i]
    vec<-as.character(vec)%>%as.factor()
    le<-levels(vec)[1:10]
    sign<-c(sign,le)
  }
  sign<-as.factor(sign)
  print(levels(sign)[1:n])
}




#' set those strange values to NA as a missing value
#'
#' This function set those strange values to NA as a missing value
#'
#' @param df the dataframe
#' @param strange.list the strange sign or values
#' @export
#' @examples
#' stran_sign_handle(p2p,c('-',"$"))
stran_sign_handle<-function(df,strange.list,allchar=F){
  strange.list<-as.character(strange.list)
  for(i in names(df)){
    type<-class(df[,i])
    df[,i]<-as.character(df[,i])
    ssp<-df[,i]%in%strange.list%>%which()
    df[ssp,i]<-NA
    if(!allchar){
      if(type=='factor')
        df[,i]<-as.factor(df[,i])else if(type=='numeric')
          df[,i]<-as.numeric(df[,i])
    }
  }
  return(df)
}

