library(xgboost)
## filter variables
allvars<-alldata%>%names
# useful <- rpart_filter(alldata,'Y',controls=tree.control)
useful <- allvars[!allvars%in%c('label','user_id')]
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
    max.depth = 2,
    colsample_bytree = 0.5,
    min_child_weight = 100,
    subsample = 0.7,
    eta = 0.01,
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
    nround = 10000,
    verbose = 1,
    watchlist = list('train' = dtrain, 'validation' = dtest)
  )

var.imp <- xgb.importance(useful, model = bst)
score<-predict(bst,dpred)
 