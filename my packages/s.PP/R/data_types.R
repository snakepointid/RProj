#' get a variables' distinct values matrix
#'
#' get a variables' distinct values matrix. this function is typically used to determine 
#' those discrete variables whose distinct values' number is more than a threshold
#'
#' @param df the dataframe
#' @param var.list the variables to be show,default: all variables
#' @export
#' @examples
#' count_distinct_value(p2p)

count_distinct_value<-function(df,var.list=names(df)){
  n<-1
  for(i in var.list){
    vec<-df[,i]
    temp<-table(vec)%>%names()
    l<-length(temp)
    va.vec<-c(i,l,temp[1],temp[l])
    if(n==1)va.df<-va.vec else
      va.df<-rbind(va.df,va.vec)
    n<-n+1
  }
  va.df<-data.frame(va.df,row.names = NULL)
  va.df[,2]<-va.df[,2]%>%as.character()%>%as.numeric()
  names(va.df)<-c("colnames","numbers","first.value","last.value")
  va.df<-va.df[order(va.df[,2]),]
  return(va.df)
}
#' define the columns' data type
#'
#' This function define the variable's data type by variable's distinct values' numbers
#'
#' @param df the dataframe
#' @param vars those variables whose distinct values' numbers is more than a threshold but still need to be discrete type a the maxmimum distinct values' numbers that a discrete variable could have
#' @param type_fun choose type
#' @export 
#' @examples
#' dis.list<-"auth_key_relation"
#' define_types(p2p,as.character,3)

define_types<-function(df,type_fun,vars=NULL){
  if(is.null(vars))
    vars<-names(df)
  vars<-vars[vars%in%names(df)]
  for(i in vars){
    df[,i]<-as.character(df[,i])%>%type_fun()
  }
  return(df)
}

#' seperate the variables into discrete and numeric groups
#'
#' seperate the variables into discrete and numeric groups
#'
#' @param df the dataframe
#' @param targ the label variable
#' @export
#' @examples
#' temp<-get_types(p2p,"def")
#' dis.vars<-temp[["dis"]]
#' con.vars<-temp[["con"]]

get_types<-function(df,targ){
  dis<-c()
  cont<-c()
  other<-c()
  for(i in names(df)){
    if(i!=targ){
      if(is.factor(df[,i]))
        dis<-c(dis,i)else if(is.numeric(df[,i])) 
          cont<-c(cont,i)else
            other<-c(other,i)
    }
  }
  return(list('discrete'=dis,'continue'=cont,'other'=other))
}
#' define the columns' data type accoding to predefined types doc
#'
#' This function define the columns' data type accoding to predefined doc
#'
#' @param df the dataframe
#' @param pre.types predefined types doc
#' @export 
#' @examples
#' 
#' define_types_by_predoc(p2p,pre.types)

define_types_by_predoc<-function(df,pre.types){
  unpre<-c()
  mvc<-c()
  for(i in names(df)){
    print(paste('now handle',i))
    tryCatch({ 
      df[,i]<-as.character(df[,i])
      mp<-which(pre.types[,1]==i)
      if(length(mp)==1){
        type<-pre.types[mp,2]
        if(type%in%c("numeric","int","float","double",0)){
          df[,i]<-as.numeric(df[,i])
        }else if(type%in%c("category","discrete","string",1)){
          luv<-length(unique(df[,i]))
          if(luv>20){
            names(i)<-luv
            mvc<-c(mvc,i)
            warning(print(paste(i," DVDVTM"))
            )
          }else
            df[,i]<-as.factor(df[,i])
        }else if(type%in%c("ID","num",'idx','id','time','date',3)){
            df[,i]<-as.character(df[,i])
        }else{
          stop("PDTNE")
        }
      }else{
        unpre<-c(unpre,i)
      } 
    },warning = function(w) {
      mp<-which(!is.na(df[,i]))[1:5]
      l<-length(unique(df[,i]))
      paste(i,"unique values is",l)%>%print()
      print(df[mp,i])
      print('--------------------------')
    })
  }
  if(length(unpre)>0){
    print(unpre)
  }
  mvc<-mvc[order(as.numeric(names(mvc)),decreasing = T)]
  return(list('df'=df,'undifine'=unpre,'mvc'=mvc))
}