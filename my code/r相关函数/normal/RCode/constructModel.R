library(xgboost)
## filter variables
allvars<-alldata%>%names
 
useful <- allvars[!allvars%in%c('label','user_id','targwindowday')]
 
all.y<-alldata$label
 
predSamp<-which(all.y==-1)
trainTestSamp<-which(all.y!=-1)

train.samp <- sample(trainTestSamp,0.7*length(trainTestSamp),replace = F)
test.samp  <-trainTestSamp[!trainTestSamp%in%train.samp]
#----------------------------------------------------------------------------------------------------------------------#
##  get train and test datas
traindata <- alldata[train.samp,useful]%>%data.table
testdata  <- alldata[test.samp,useful]%>%data.table
preddata<-alldata[predSamp,useful]%>%data.table
 
train.y    <- all.y[train.samp]
test.y     <- all.y[test.samp]
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
 
outfile<-data.table(user_id=alldata[predSamp,]$user_id,score)
outfile<-outfile[score>0.5,]
outfile<-outfile[,.(user_id,sku_id=score)]
outfile[,"sku_id"]<-154636
 
write.csv(outfile,"/Users/snakepointid/Documents/project/JDproj/result/result0404.csv",row.names = F,fileEncoding = "UTF-8")
 
