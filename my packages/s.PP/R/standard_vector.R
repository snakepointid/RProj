#' scale vector
#' 
#' scale vector
#'
#' @param df the dataframe
#' @param vars the candidates variables
#' @param type the scale type
#' @export
#' @examples
#' stand_fit(p2p,con.vars,type='sdm')
stand_fit<-function(df,vars,type="maxmin"){
  minus<-c()
  divide<-c()
  if(type=="maxmin"){
  for(i in vars){
    vec<-df[,i]
    mi<-min(vec,na.rm = T)
    di<-max(vec,na.rm = T)-mi
    minus<-c(minus,mi)
    divide<-c(divide,di)
  }}else{
  for(i in vars){
    vec<-df[,i]
    mi<-mean(vec,na.rm = T)
    di<-sd(vec,na.rm = T)
    minus<-c(minus,mi)
    divide<-c(divide,di)
  }
  }
  st.df<-data.frame(vars=vars,minus=minus,divide=divide,stringsAsFactors=FALSE)
  return(st.df)
}
#' transform scale vector
#' 
#' transform scale vector
#'
#' @param df the dataframe
#' @param fit the fitted scaler model
#' @export
#' @examples
#' stand_transform(p2p,st.fit)
stand_transform<-function(df,fit){
  vars<-names(df)
  for(i in 1:length(fit[,1])){
    var<-fit[i,1]
    if(var%in%vars){
      temp<-df[,var]
      df[,var]<-(temp-fit[i,2])/fit[i,3]
    }
  }
  return(df)
}
