#' change the variable value from discrete to continue
#'
#' change the variable value from discrete to continue
#' 
#' @param df the dataframe
#' @param ori the original variable
#' @export
#' @examples
#' 
#' decode_type(df,ori)
decode_type<-function(df,ori){
  temp<-df[,ori]
  temp[df[,ori]==99]<-NA
  temp[df[,ori]==56]<-15000000
  temp[df[,ori]%in%47:55]<-(temp[df[,ori]%in%47:55] -47)*1000000+1500000
  temp[df[,ori]%in%38:46]<-(temp[df[,ori]%in%38:46] -38)*100000+150000
  temp[df[,ori]%in%20:37]<-(temp[df[,ori]%in%20:37] -20)*5000+12500
  temp[df[,ori]%in%0:19]<-(temp[df[,ori]%in%0:19] -0)*500+250
  return(temp)
}
#' change the numeric info to understanding info
#'
#' change the numeric info to understanding info
#' 
#' @param vec the numeric info
#' @param predoc the pre-defined code info
#' @export
#' @examples
#' 
#' decode_id_phone(vec,predoc)
decode_id_phone<-function(tdf,predoc){
  names(predoc)<-c('id','value')
  tdf[,1]<-as.character(tdf[,1])
  predoc[,1]<-as.character(predoc[,1])
  temp<-left_join(tdf,predoc, by ="id")
  return(temp[,2])
}

#' combine two features into a new feature
#'
#' combine two features into a new feature
#' 
#' @param df the dataframe
#' @param oris the original variables
#' @param a change continue to discrete
#' @export
#' @examples
#' 
#' combine_type(df,oris,0)
combine_type<-function(df,oris,a=NA){
  temp<-df
  temp[is.na(temp[,oris])]<-0
  temp[,oris[1]]<-as.numeric(temp[,oris[1]]>a)
  temp[,oris[2]]<-as.numeric(temp[,oris[2]]>a)
  t.t<-paste(temp[,oris[1]],temp[,oris[2]],sep='')
  tt<-t.t
  tt[which(t.t=='01')]<-1
  tt[which(t.t=='10')]<-2
  tt[which(t.t=='11')]<-3
  tt[which(t.t=='00')]<-4
  return(tt%>%as.numeric())
}

#' get the mean,sum,max or min of many features
#'
#' get the mean,sum,max or min of many features
#' 
#' @param df the dataframe
#' @param oris the original variables
#' @param type mean,sum or max..
#' @export
#' @examples
#' 
#' agg_type(df,oris,type)
agg_type<-function(df,oris,type){
  if(length(oris)==1){
    temp<-df[,oris]
    return(temp)
  }else{
  temp<-apply(df[,oris], 1, function(s)length(which(is.na(s))))
  mp<-which(temp==length(oris))
  if(type=='wmax'|type=='wmin'){
    df<-data.frame(df[,oris],min(df[,oris],na.rm = T)-1)
    temp<-unlist(apply(df,1,function(s)agg.types[[type]](s)))
    temp[temp==length(oris)+1]<-NA
  }else if(type=='wrec'){
    temp<-apply(df[,oris],1,function(s)min(which(!is.na(s))))
    temp<-temp-1
  }else if(type=='wrec.b'){
    temp<-apply(df[,oris],1,function(s)min(which(s>0)))
    temp<-temp
  }else{
    temp<-apply(df[,oris],1,function(s)agg.types[[type]](s,na.rm=T))
  }
  temp[mp]<-NA
  temp[is.infinite(temp)]<-NA
  return(temp)}
}
#' get the ratio type
#'
#' get the ratio type
#' 
#' @param df the dataframe
#' @param oris the original variables
#' @export
#' @examples
#' 
#' agg_type(df,oris,type)
tr_type<-function(df,oris){
  vec1<-df[,oris[1]] 
  vec2<-df[,oris[2]] 
  temp<-rep(0,length(vec1))
  temp[which(is.na(vec1)&!is.na(vec2))]<-1
  temp[which(!is.na(vec1)&is.na(vec2))]<-2
  temp[which(vec2/vec1<1)]<-3
  temp[which(vec2/vec1==1)]<-4
  temp[which(vec2/vec1>1)]<-5
  return(temp)
}

#' change continue features into discrete features
#'
#' change continue features into discrete features
#' 
#' @param df the dataframe
#' @param oris the original variables
#' @param a the threshold
#' @export
#' @examples
#' 
#' c2d_type(df,oris,a)
c2d_type<-function(df,oris,a){
  if(length(a)>1){
    a<-a[order(a)]
    for(i in oris){
      vec<-df[,i]
      temp<-vec
      vec[temp<=a[1]]<-2
      vec[temp>a[1]&temp<a[2]]<-0
      vec[temp>=a[2]]<-1
      df[,i]<-vec
    }}else{
      for(i in oris){
        df[,i]<-as.numeric(df[,i]>a)
      }
    }
  return(df[,oris])
}
#' change continue features into discrete features
#'
#' change continue features into discrete features
#' 
#' @param df the dataframe
#' @param oris the original variables
#' @param a the threshold
#' @export
#' @examples
#' 
#' c2d_type(df,oris,a)
encode_cate<-function(vars,type='cons'){
  code<-c()
  codes<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11",
           "C12","C13","C14","C15","C16","C17","C18","C19","C20","C21",
           "C22","C23","C24","C25","C26","C27","C28","C29")
  names(codes)<-c("BDSH","BXLC","CCLY","CWSH","DNBG","FC","FZPS","GHHZ","JJJC",
                  "JJJF","JYDQ","JYPX","MSTC","MYYP",
                  "QCYP","QT","RYBH","SC","SJSJPJ","SM","TX","WHYL","WLYXXNWP"
                  ,"X","XB","YDHW","YLBJ","ZBGJS","ZBSS")
  if(type=='cons'){
    for(i in vars){
      code<-c(code,codes[i])
    }
  }
  return(code)
}
