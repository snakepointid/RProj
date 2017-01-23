#' get a robust information value of a single variable
#'
#' This function generatea  the variable's information values by a robust way
#' 
#'
#' @param prob the variable that need to compute
#' @param target the label
#' @param lowest the lowest samples that must have to make the iv robust
#' @export
#' @examples
#' get_IV(p2p[,1],p2p[,"def"],500)

get_IV<-function(prob,target,lowest=500,miss=T){
  prob<-cut(prob,unique(c(Inf,-Inf,quantile(prob,(1:19)*0.05))))
  if(miss)
    prob<-miss_handle(prob,-9999)
  temp<-leave_0_point(table(target,prob),T,lowest)
   
  bad.vec<-temp[2,]
  good.vec<-temp[1,]
  
  bads<-sum(bad.vec)
  goods<-sum(good.vec)
  
  bad.woe<-bad.vec/bads
  good.woe<-good.vec/goods
  woe<-log(bad.woe/good.woe)
  dif<-bad.woe-good.woe
  ivi<-dif*woe
  iv<-sum(ivi)
  sep_nums<-length(ivi)
  if(is.nan(iv))
    iv<-0
  return(c('iv'=iv,'sn'=sep_nums))
}

#' get the p values of single variable
#'
#' This function using a single variable logistic regression to estimate the p value
#'
#' @param prob the variable that need to compute
#' @param target the label
#' @export
#' @examples
#' get_pvalue(p2p[,1],p2p[,"def"])

get_pvalue<-function(prob,target){
  tryCatch({
    if(is.factor(target))
      temp=glm(target~prob,family = binomial(link="logit"))else
        temp=glm(target~prob,family = gaussian(link = "identity"))
    temp=summary(temp)
    temp=temp$coefficients[2,4]
    return(temp)
  },error=function(e){
    return(1)
  },warning=function(w){
    return(1)
  }
  )
}
#' get the information values of all colunms
#'
#' This function generatea all the information values of dataframe's columns
#' 
#'
#' @param df the dataframe
#' @param vars columns which need to compute;default:all columns will be used
#' @param target the target columns
#' @param lowest the lowest samples that must have to make the iv robust
#' @export
#' @examples
#' get_df_info(df =p2p,targ='def',lowest=500)

get_df_info<-function(df,targ,lowest,vars=names(df)){
  vars<-vars[vars!=targ]
  InFo<-c()
  for(i in vars){
    tryCatch({
      iv<-get_IV(df[,i],df[,targ],lowest)
      names(iv)<-i
      InFo<-c(InFo,iv)
      paste('have already computed the iv of',i)%>%print()
    },error=function(e){
      paste("can't get iv of",i)%>%print()
    }
    )
  }
  InFo<-InFo[order(InFo,decreasing = T)]
  return(InFo)
}

#' transform the variable to a more robust stucture
#'
#' This function generatea transform the variable to a more robust stucture
#' the function will remove those seperations that have no positive or negtive 
#' samples or the all samples numbers is too small
#' 
#'
#' @param table.matrix cross tabulation of two variables
#' @param dis boolean; if true,then reorder the table according to the odds of positive and negtive
#' @param lowest the smallest sample numbers that seperation must have
#' @export
#' @examples
#' leave_0_point(tables,dis=T,lowest=500)

leave_0_point<-function(table.matrix,dis=F,lowest){
  odds<-table.matrix[2,]/table.matrix[1,]
  table.matrix=rbind(table.matrix,odds)
  all<-table.matrix[2,]+table.matrix[1,]
  table.matrix=rbind(table.matrix,all)
  if(dis){
    table.matrix = table.matrix[,order(table.matrix[3,])]
  }
  mp<-which(table.matrix[3,]==0|is.infinite(table.matrix[3,])|table.matrix[4,]<lowest)
  while(length(mp)>0){
    if(length(table.matrix[4,])<3)break
    drop = mp[1]
    ncol = length(table.matrix[3,])
    if(drop==1) {
      add=2}else if(drop==ncol){ 
        add=ncol-1}else{
          add=drop-1
          if(table.matrix[4,drop-1]>table.matrix[4,drop+1]){
            add=drop+1
          }
        }
    table.matrix[,add]=table.matrix[,add]+table.matrix[,drop]
    table.matrix=table.matrix[,-drop]
    table.matrix[3,]<-table.matrix[2,]/table.matrix[1,]
    
    mp<-which(table.matrix[3,]==0|is.infinite(table.matrix[3,])|table.matrix[4,]<lowest)
  }
  table.matrix<-table.matrix[-c(3,4),]
  return(table.matrix)
}
#' remove the redundant variables
#'
#' This function compute the correlation rate of two variables 
#' and remove the variables that its information value is less
#' 
#' @param df the dataframe
#' @param cor.rate the correlation threshold
#' @param num.vec must be numeric variables
#' @param info.vec the information vector 
#' @param method there are three ways to compute correlation rate
#' @export
#' @examples
#' cor_test(p2p,0.7,con.vars,iv.list)
cor_test<-function(df,cor.rate,info.vec,method="spearman"){
  useless<-c()
  var.vec<-names(info.vec)
  cor.matrix<-cor(df[,var.vec],method=method)%>%abs()
  drop.p<-apply(cor.matrix,2,function(s)which(s>cor.rate))
  for(i in 1:length(var.vec)){
    if(i%in%useless)next
    temp<-drop.p[[i]]
    useless<-c(useless,temp[temp>i])}
  return(unique(var.vec[useless]))
}

