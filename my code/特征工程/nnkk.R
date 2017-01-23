#' get a distance dataframe
#'
#' This function will get a distance dataframe
#'
#' @param df dataframe need to be transform
#' @param nparts the number of partners to compute distance
#' @param l the abs or squar
#' @export
#' @examples
#' get_dist_df(p2p,5)
get_dist_df<-function(df,nparts,ln){
  df<-as.matrix(df)
  l<-length(df[,1])
  df<-df[sample(1:l,l,replace = F),]
  i<-1
  idx<-0
  while(l-i>=nparts){
    print(i)
    part<-sample((i+1):l,nparts,replace=F)
    temp<-t(apply(df[part,], 1,function(s)s-df[i,]))
    if(i==1)
      ddf<-temp else
        ddf<-rbind(ddf,temp)
    i<-i+1
  }
  ddf<-abs(ddf)^ln
  ddf<-as.data.frame(ddf)
  return(ddf)
}
#' transform the dataframe into all numeric dataframe
#'
#' This function will transform the dataframe into all numeric dataframe
#'
#' @param df dataframe need to be transform
#' @param fit the fitted transform rule
#' @export
#' @examples
#' dis2con_transform(p2p,dfit)
dis2con_transform<-function(df,fit){
  vars<-names(df)
  for(i in names(fit)){
    if(i %in% vars){
      fits<-fit[[i]]
      vec<-as.character(df[,i])
      temp<-vec
      for(j in 1:length(fits)){
        vec[temp==fits[j]]<-j-1
      }
      vec<-vec%>%as.numeric()
      vec[vec!=0]<-1
      df[,i]<-vec
    }
  }
  return(df)
}
#' get a dis2con transformation fit
#'
#' This function will get a dis2con transformation fit
#'
#' @param df dataframe need to be transform
#' @export
#' @examples
#' dis2con_fit(p2p)
dis2con_fit<-function(df){
  fit<-list()
  for(i in names(df)){
    if(is.factor(df[,i])){
    vec<-as.character(df[,i])
    dis<-unique(vec)
    fit[[i]]<-dis
    }
  }
  return(fit)
}

#' get a dis2con transformation fit
#'
#' This function will get a dis2con transformation fit
#'
#' @param df dataframe need to be transform
#' @export
#' @examples
#' dis2con_fit(p2p)
knn_predict<-function(test,train,targ,model,l,k){
  test<-as.matrix(test)
  train<-as.matrix(train)
  n<-length(test[,1])
  probs<-c()
  for(i in 1:n){
    print(i)
    temp<-t(apply(train, 1,function(s)s-test[i,]))
    prob<-predict(model,newdata=abs(temp)^l)
    pred_s<-train[order(prob),targ][1:k]
    pred_d<-abs(train[order(-prob),targ][1:k]-1)
    prob<-mean(c(pred_d,pred_s))
    probs<-c(probs,prob)
  }
  return(probs)
}


