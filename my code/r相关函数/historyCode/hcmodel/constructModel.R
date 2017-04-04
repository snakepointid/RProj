library(xgboost)
## filter variables
allvars<-alldata%>%names
# useful <- rpart_filter(alldata,'Y',controls=tree.control)
useful <- allvars[!allvars%in%c('label','user_id','targwindowday')]
sampSamp<-c(1:length(alldata$user_id))
train.samp <- sample(sampSamp,0.7*length(sampSamp))
test.samp<-sampSamp[!sampSamp%in%train.samp]
 
all.y<-alldata$label
train.y    <- all.y[train.samp]
test.y     <- all.y[test.samp]
alldata<-alldata[,useful]%>%data.table
preddata<-predictDF[,useful]%>%data.table
#----------------------------------------------------------------------------------------------------------------------#
##  get train and test datas
traindata <- alldata[train.samp,]
testdata  <- alldata[test.samp,]
#----------------------------------------------------------------------------------------------------------------------#
##  construct model
####  filter those variables by using decision tree
####  train the model by using xgboost
#----------------------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------------------------#
##  transfer the dataframe to the type xgboost need
###  trainset
train.mat <- do.call(cbind, traindata )
train.l   <- train.y%>% as.character() %>% as.numeric()
dtrain    <- xgb.DMatrix(data = train.mat,label = train.l)
###  testset
test.mat  <- do.call(cbind, testdata )
test.l    <- test.y %>% as.character() %>% as.numeric()
dtest     <- xgb.DMatrix(data = test.mat,label = test.l)
###  predset
pred.mat  <- do.call(cbind, preddata )
dpred     <- xgb.DMatrix(data = pred.mat)
#----------------------------------------------------------------------------------------------------------------------#
###  set the hyperparameters for xgboost
hyperPara <-
  list(
    max.depth = 1,
    colsample_bytree = 0.5,
    min_child_weight = 100,
    subsample = 0.7,
    eta = 0.07,
    objective = "binary:logistic",
    nthread = 4,
    eval_metric = 'auc'
  )
### train the model
set.seed(123)
bst <-
  xgb.train(
    params = hyperPara,
    data = dtrain,
    nround = 1000,
    verbose = 1,
    watchlist = list('train' = dtrain, 'validation' = dtest)
  )
var.imp[1:100,]
var.imp <- xgb.importance(useful, model = bst)
score<-predict(bst,dpred)
outfile<-data.table(user_id=predictDF$user_id,score)
outfile<-outfile[score>0.44,]
subdf<-actionInfo[user_id%in%outfile$user_id&cate==8&windowday%in%c(as.Date("2016-04-11"),as.Date("2016-04-06")),]
 
out<-subdf[,.(sku_id=findSku(sku_id)),.(user_id)]
write.csv(out,"/Users/snakepointid/Documents/project/JDproj/result/results0401pm3.csv",row.names = F,fileEncoding = "UTF-8")
str(out)
