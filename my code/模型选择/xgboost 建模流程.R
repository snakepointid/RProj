m.v<--9999
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- KsValue(preds,labels)
  return(list(metric = "scoring", value = err))
}
best.para<-list(max.depth =8,scale_pos_weight=0.3,lambda=1,
                gamma=1,colsample_bytree=0.6,min_child_weight=0.1,
                subsample=0.5,eta = 0.02,objective="binary:logistic",
                nthread=4,eval_metric=evalerror)
##################################################################
train.mat <- sparse.model.matrix(~.,data=traindata[,useful])
train.l<-traindata[,'default']%>%as.character()%>%as.numeric()
dtrain <- xgb.DMatrix(data =train.mat,label = train.l,missing = m.v)

vali.mat <- sparse.model.matrix(~.,data=validata[,useful])
vali.l<-target_y%>%as.character()%>%as.numeric()
dvali <- xgb.DMatrix(data =vali.mat,label = vali.l,missing = m.v)
#################################
set.seed(123)
bsts <- xgb.cv(params=best.para, data=dtrain,nround =100,verbose = T,nfold=4)
###################################
set.seed(123)
bst <- xgb.train(params=best.para, data=dtrain,nround =888, verbose = 1,watchlist=list('train'= dtrain,'validation'=dvali))
#################################
test.mat <- sparse.model.matrix(~.,data=testdata[,useful])
dtest <- xgb.DMatrix(data =test.mat,missing = m.v)
probs <- predict(bst,newdata=dtest)
#################################
outcomes<-data.frame(row.names(testdata),probs)
names(outcomes)<-c('uid','scores')
write.csv(outcomes,file='C:/Users/uij/Documents/R/my project/corporation contest 2e/docs/outcome329final.csv', row.names = F, fileEncoding = "utf-8")

