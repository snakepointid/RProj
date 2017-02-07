#' train the tree
#'
#' @param x the dataframe
#' @param y the target variable
#' @export
#' @examples
#' streeTrain(x,y,maxdepth=3,minisamp=30)

streeTrain<-function(x,y,maxdepth=3,minisamp=30)
{
  maxdepth<<-maxdepth
  minisamp<<-minisamp
  yfit<-rep(0,length(y))
  yidx<-1:length(y)
  nodes<<-list()
  splitTheNode(x,y,yidx,'0')
  return(nodes)
}
#' split the dataframe and get the node
#'
#' This function is mainly split the dataframe and get the node
#'
#' @param x the dataframe
#' @param y the target variable
#' @param yidx the index of the target variable y
#' @export 
#' @examples
#' splitTheNode(x,y,idx,'0')
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
#' choose the random feature to split and get the optimal segmentation point 
#'
#' this function mainly choose the random feature to split and get the optimal segmentation point 
#' @param prob the feature which is choosed randomly
#' @param target the target variable
#' @export
#' @examples
#' split(prob,target)
split <- function(prob,target)
{
  idx <- order(prob)
  prob <- prob[idx]
  postarget <- target[idx]
  ks <- abs(cumsum(postarget))
  midx <- which.max(ks)
  return(c(ks[midx],prob[midx]))
}
#' predict the target y value  
#'
#' This function mainly predict the target y value 
#'
#' @param x the dataframe
#' @param snakeTree the nodes which is from the above 
#' @export 
#' @examples
#' 
#' predict(x,snakeTree)
predict<-function(x,snakeTree)
{
  obs<-length(x[,1])
  ypred<<-rep(0,obs)
  nodes<<-snakeTree
  yidx<-1:obs
  predictY(x,yidx,'0')
  return(ypred)
}
#' predict the y in each node 
#'
#' This function mainly predict the y value in each node 
#'
#' @param x the dataframe
#' @param yidx the index of the target variable 
#' @export 
#' @examples
#' 
#' predictY(x,yidx,code)
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
