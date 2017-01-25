#' filter the features
#'
#' using single variable's information to choose the features;
#' discrete variables using iv as the critic and numeric variables use the significans,Pvalue as the evaluation critics;
#' using a iv threshold to choose those variables which are bigger than it;
#' and the numeric variables will be filtered by correlation test fothermore;
#'
#' @param df the dataframe
#' @param targ the label variable
#' @param lowest the minimum seperation sample numbers to get a robust information
#' @param method the correlation test methods<spearson,pearson and kendall>
#' @param cor.rate the correlation threshold
#' @param iv.level the discrete variables' iv threshold
#' @param sig.level the numeric variables' p value threshold 
#' @export
#' @examples
#' sig_filter(p2p,targ="def",iv.info,lowest=500,method="spearman",cor.rate=0.7,dis.level=0.02,sig.level=0.05)
sig_filter<-function(df,targ,iv.info,sig.info,lowest=500,method="spearman",cor.rate=0.7,iv.level=0.02,sig.level=0.05){
  set.seed(123)
  temp<-get_types(df,targ)
  dis.var<-temp[[1]]
  con.var<-temp[[2]]
  use.dis<-c()
  use.con<-c()
  useless<-c()
  if(length(dis.var)>0){
  iv.dis<-iv.info[names(iv.info)%in%dis.var]
  use.dis<-names(iv.dis[iv.dis>iv.level])}
  if(length(con.var)>0){
  sig.con<-sig.info[names(sig.info)%in%con.var]
  sig.con<-sig.con[sig.con<sig.level]
  use.con<-names(sig.con)
  useless<-cor_test(df,cor.rate,sig.con,method)
  }
  useful<-c(use.dis,use.con)
  useful<-useful[!useful%in%useless]
  return(useful)
  }


#' filter the features
#'
#' using single variable's information to choose the features;
#' both discrete variables and numeric variables use the iv to determine their information;
#' using a iv threshold to choose those variables which are bigger than it;
#' and the numeric variables will be filtered by correlation test fothermore;
#'
#' @param df the dataframe
#' @param targ the label variable
#' @param lowest the minimum seperation sample numbers to get a robust information
#' @param method the correlation test methods<spearson,pearson and kendall>
#' @param cor.rate the correlation threshold
#' @param dis.level the discrete variables' iv threshold
#' @param con.level the numeric variables' iv threshold,generally is bigger than discrete variables' 
#' @export
#' @examples
#' iv_filter(p2p,targ="def",lowest=500,method="spearman",cor.rate=0.7,dis.level=0.02,con.level=0.04)

iv_filter<-function(df,targ,iv.info,lowest=500,method="spearman",cor.rate=0.7,dis.level=0.02,con.level=0.04){
  set.seed(123)
  temp<-get_types(df,targ)
  dis.var<-temp[[1]]
  con.var<-temp[[2]]
  use.dis<-c()
  use.con<-c()
  useless<-c()
  if(length(dis.var)>0){
  iv.dis<-iv.info[names(iv.info)%in%dis.var]
  use.dis<-names(iv.dis[iv.dis>dis.level])
  }
  if(length(con.var)>0){
  iv.con<-iv.info[names(iv.info)%in%con.var]
  iv.con<-iv.con[iv.con>con.level]
  use.con<-names(iv.con)
  useless<-cor_test(df,cor.rate,iv.con,method)
  }
  
  useful<-c(use.dis,use.con)
  useful<-useful[!useful%in%useless]
  return(useful)}
#' filter the features by decision tree
#'
#' using decision tree to choose the features
#'
#' @param df the dataframe
#' @param targ the label variable
#' @param controls the rpart tree's parameters 
#' @export
#' @examples
#' rpart_filter(p2p,"def",controls=rpart.control(minsplit = 100,
#'                        maxdepth = 10, cp = 0, maxcompete = 1,
#'                       maxsurrogate = 1, usesurrogate = 1, 
#'                       xval =4,surrogatestyle = 0))

rpart_filter<-function(df,targ,controls=rpart.control(minsplit = 100, 
                              maxdepth = 10, cp = 0,maxcompete = 1, 
                              maxsurrogate = 1, usesurrogate = 1, 
                              xval =4,surrogatestyle = 0)){
rpart<-rpart(as.formula(paste(targ,"~.")),data=df,control=controls)
useful<-names(rpart$variable.importance)
return(useful)}

#' filter the features
#'
#' using single variable's missing information to choose the features;
#' both discrete variables and numeric variables use the iv to determine their information;
#' using a iv threshold to choose those variables which are bigger than it;
#' and the numeric variables will be filtered by correlation test fothermore;
#'
#' @param df the dataframe
#' @param targ the label variable
#' @param lowest the minimum seperation sample numbers to get a robust information
#' @param method the correlation test methods<spearson,pearson and kendall>
#' @param cor.rate the correlation threshold
#' @param dis.level the discrete variables' iv threshold
#' @param con.level the numeric variables' iv threshold,generally is bigger than discrete variables' 
#' @export
#' @examples
#' miss_filter(df,iv.info,low.ml=0.05,high.ml=0.8,both.ml=0.5,pvl=0.001)

miss_filter<-function(df,iv.info,low.ml=0.05,high.ml=0.8,both.ml=0.5,pvl=0.001){
  useless<-c()
  vars<-names(iv.info)
  for(i in vars){
    imp<-is.na(df[,i])%>%which()
    smr<-length(imp)/length(df[,i])
    if(i %in% useless|smr<lml)
      next
    if(smr>hml){
      print(paste(i,' do not keep',sep=''))
      useless<-c(useless,i)
      next
    }
    for(j in vars){
      if(j==i)
        next
      jmp<-is.na(df[,j])%>%which()
      smrj<-length(jmp)/length(df[,j])
      if(j %in% useless|smrj<lml)
        next
      bmpi<-imp[imp%in%jmp]
      bmpj<-jmp[jmp%in%imp]
      bmr<-min(length(bmpi)/length(imp),length(bmpj)/length(jmp))
      pv<-get_pvalue(df[,j],df[,i])
      if(bmr>bml&pv<pvl){
        print(paste(j,' do not keep',sep=''))
        useless<-c(useless,j)
      }
    }
  }
  useful<-vars[!vars%in%useless]
  return(useful)
}