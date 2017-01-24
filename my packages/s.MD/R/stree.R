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

streeTrain<-function(x,y,maxdepth=3,minisamp=30)
{
  yfit<-rep(0,length(y))
  yidx<-1:length(y)
  nodes<<-list()
  splitTheNode(x,y,yidx,'0')
  return(nodes)
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
splitTheNode<-function(x,y,yidx,code)
{
  if(nchar(code)>maxdepth|length(unique(y))==1|length(y)<minisamp)
  { 
    tmp = mean(y,na.rm=T)
    yfit[yidx] <<- tmp
    nodes[[code]] <<- c(F,tmp)
  }else
  {
    featureIdx     = sample(1:length(x[1,]),1)          
    out            =  split(x[,featureIdx],y) 
    nodes[[code]]  <<- c(T,featureIdx,out[2])
    leftidx        = x[,featureIdx]<=out[2] 
    rightidx       = x[,featureIdx]>out[2]       
    splitTheNode(x[leftidx,],y[leftidx],yidx[leftidx],paste(code,'0',sep=''))
    splitTheNode(x[rightidx,],y[rightidx],yidx[rightidx],paste(code,'1',sep=''))
  }
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
split <- function(prob,target)
{
  idx <- order(prob)
  prob <- prob[idx]
  postarget <- target[idx]
  ks <- abs(cumsum(postarget))
  midx <- which.max(ks)
  return(c(ks[midx],prob[midx]))
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
predict<-function(x,snakeTree)
{
  obs<-length(x[,1])
  ypred<<-rep(0,obs)
  nodes<<-snakeTree
  yidx<-1:obs
  predictY(x,yidx,'0')
  return(ypred)
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
predictY<-function(x,yidx,code)
{
  node<-nodes[[code]]
  
  if(node[1]==0)
  {
    ypred[yidx] <<- node[2]
  }
  else
  {
    featureIdx<-node[2]
    split<-node[3]
    leftidx  =x[,featureIdx]<=split  
    rightidx =x[,featureIdx]> split  
    predictY(x[leftidx],yidx[leftidx],paste(code,'0',sep=''))       
    predictY(x[rightidx],yidx[rightidx],paste(code,'1',sep=''))  
  }
}