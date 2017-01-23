#' get a more robust discrete vector
#'
#' This function will get a more robust discrete vector by remove
#'  those samples with only postive or negtive or sum of them is too small
#' 
#'
#' @param predict the discrete variable
#' @param target the label variable
#' @param lowest the lowest samples that must have to make the iv robust
#' @export
#' @examples
#' dis_cut(p2p[,1],p2p[,"def"],500)
dis_cut<-function(predict,target,lowest){
  table.matrix<-table(target,predict)
  if(length(table.matrix[1,])==1)
    return(names(table(predict)))
  odds<-table.matrix[2,]/table.matrix[1,]
  table.matrix=rbind(table.matrix,odds)
  all<-table.matrix[2,]+table.matrix[1,]
  table.matrix=rbind(table.matrix,all)
  table.matrix = table.matrix[,order(table.matrix[3,])]
  v.b<-names(table.matrix[1,])
  mp<-which(table.matrix[3,]==0|is.infinite(table.matrix[3,])|table.matrix[4,]<lowest)
  while(length(mp)>0){
    if(length(table.matrix[4,])<3)
      break
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
    v.b[add]<-paste(v.b[add],"&",v.b[drop],sep="")
    table.matrix[,add]<-table.matrix[,add]+table.matrix[,drop]
    v.b<-v.b[-drop]
    table.matrix=table.matrix[,-drop]
    table.matrix[3,]<-table.matrix[2,]/table.matrix[1,]
    mp<-which(table.matrix[3,]==0|is.infinite(table.matrix[3,])|table.matrix[4,]<lowest)
  }
  return(v.b)
}
#' get a more robust numeric vector
#'
#' This function will get a more robust numeric vector by remove
#'  those samples with only postive or negtive or sum of them is too small
#' 
#'
#' @param predict the numeric variable
#' @param target the label variable
#' @param lowest the lowest samples that must have to make the iv robust
#' @export
#' @examples
#' num_cut(p2p[,1],p2p[,"def"],500)
num_cut<-function(predict,target,lowest){
  values<-names(table(predict))%>%as.numeric()
  sep.point<-c()
  iv.globest<-0
  while(2>1){
    cut.best<-c()
    iv.locbest<-0
    for(i in values){
      cut.vec<-cut(predict,c(-Inf,Inf,sep.point,i))
      iv.temp<-cut_IV(cut.vec,target,lowest)
      if(iv.temp>iv.locbest){
        iv.locbest<-iv.temp
        cut.best<-i
      }
    }
    sep.point<-c(sep.point,cut.best)
    values<-values[values!=cut.best]
    if(iv.locbest>iv.globest+0.005){
      iv.globest<-iv.locbest
    }else{
      print(iv.globest)
      break
    }
  }
  return(sep.point)
}
#' transform the orignial vectors into a more robust vectors
#'
#' This function transform the orignial vectors into a more robust vectors
#' 
#'
#' @param df the dataframe need to be transformedd
#' @param fit the fitted transform model
#' @export
#' @examples
#' cut_transform(p2p,fit)
cut_transform<-function(df,fit){
  fit.vars<-names(fit)
  for(i in names(df)){
    if(i %in% fit.vars){
      if(is.factor(df[,i])){
        df[,i]<-as.character(df[,i])
        amp<-c()
        temp<-fit[[i]]
        for(j in temp){
          match.v<-unlist(strsplit(j, "&"))
          mp<-which(df[,i]%in%match.v)
          amp<-c(amp,mp)
          df[mp,i]<-j
        }
        df[-amp,i]<-"no.ex"
        df[,i]<-as.factor(df[,i])
      }else{
        df[,i]<-cut(df[,i],c(-Inf,Inf,fit[[i]]))
      }
    }
  }
  return(df)
}
#' get the fitted model
#'
#' This function will get those columns' transform rules
#' 
#'
#' @param df the training dataframe
#' @param targ the label
#' @param lowest the lowest samples that must have to make the iv robust
#' @export
#' @examples
#' cut_fit(p2p,"def",500)
cut_fit<-function(df,vars,targ,lowest){
  fit<-list()
  count<-0
  for(i in vars){
    tryCatch({
      if(i!=targ){
        if(is.factor(df[,i])){
          fit[[i]]<-dis_cut(df[,i],df[,targ],lowest)
        }else{
          fit[[i]]<-num_cut(df[,i],df[,targ],lowest)
        }
      }
      count<-count+1
      print(paste("now cut",i,"---already finished",count,"vars"))
    }
  ,error=function(e){
    print(paste("something wrong when cut",i))
  }
  )
  }
  return(fit)
}
cut_IV<-function(prob,target,lowest=500,miss=T){
  iv<-0
  tryCatch({
    if(miss)
      prob<-miss_handle(prob,-9999)
    dis=is.factor(prob)
    temp<-leave_0_point(table(target,prob),dis,lowest)
    
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
  },error=function(e){
    
  })
  return(iv)
}
