require(xgboost)
require(dplyr)
require(Matrix)
require(rpart)
####################################进行变量筛选
rpart<-rpart(def~.,data=p2p.model,control=controls.rpart)
useful<-names(rpart$variable.importance)
##############################################改变数据结构
train.mat <- sparse.model.matrix(def~., data = p2p.model)
train.l<-p2p.model[,"def"]%>%as.character()%>%as.numeric()
##############################################优化
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- ks(preds,labels)
  return(list(metric = "ks", value = err))
}
dtrain <- xgb.DMatrix(data =train.mat,label = train.l)
watchlist <- list(eval = dtest, train = dtrain)
param <- list(max.depth =3, eta = 0.07,objective="binary:logistic")
bst <- xgb.cv(params=param, data=dtrain,nround =100,showsd=TRUE,nfold=4,feval=evalerror)
bst$test.ks.mean%>%max()
