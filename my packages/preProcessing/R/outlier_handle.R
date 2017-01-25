#' change those value exit in test dataframe but not in train dataframe
#' 
#' change those value exit in test dataframe but not in train dataframe
#' 
#' @param train the traindf
#' @param test the testdf
#' @param targ the target
#' @param a the threshold
#' @export
#' @examples
#' df_outlier_handle(train,test)

df_outlier_handle<-function(alldata,trainkeys){
  for(i in names(alldata)){
    if(is.factor(alldata[,i])){
      alldata[!alldata[,i]%in%alldata[trainkeys,i],i]<-NA
    }
  }
  return(alldata)
}


