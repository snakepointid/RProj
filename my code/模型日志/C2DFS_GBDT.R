require(xgboost)
require(dplyr)
require(Matrix)

p2p<-filter(rawdata,other_var02!=9&other_var02!=7)
p2p<-trans2miss(p2p,-1)
########################################删除标示变量
#可删除变量"flag","other","date"系列id,opendid,mail
p2p<-data.frame(p2p,row.names = p2p[,"openid"])
p2p<-select(p2p,-contains("date"))
p2p<-select(p2p,-contains("other"))
p2p<-select(p2p,-contains("flag"))
p2p<-select(p2p,-contains("note"))
p2p<-select(p2p,-contains("brand"))
p2p<-select(p2p,-contains("level"))
p2p<-select(p2p,-contains("score"))
p2p<-select(p2p,-contains("addr"))
p2p<-select(p2p,-c(id,openid,mail,cell,name,auth_stab_mail,auth_mail))
####################################先去除完全没有的变量
useful<-muti_value(p2p,"def")
p2p<-p2p[,c(useful,"def")]

miss<-missing_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]
str(p2p)

############################################################定义类型
temp<-count_distinct_value(p2p,names(p2p))
d.l<-"auth_key_relation"
p2p<-naive_defi_type(p2p,d.l,3)
str(p2p)
####################################进行变量筛选
iv<-get_var_info(p2p,names(p2p),"def",get_IV)
useful<-names(iv[iv>0.02])
disp2p<-para_sep_vec(list(p2p,useful,500),sep_wrapper)

for(i in useful){
  table(disp2p[,i])%>%print()
  print(i)
}

p2p.model<-miss_handle(disp2p,num.handle = F)
iv<-get_var_info(p2p.model,useful,"def",get_IV)
useful<-names(iv[iv>0.02])
temp.inf<-info_matr(useful,p2p.model,"def",way=get_MI)
useful<-keep.or.drop(temp.inf[[1]],100)
##############################################改变数据结构
p2p.model<-miss_handle(p2p,num.handle=T)
temp<-random_cut(p2p.model[,c(useful,"def")],0.25)
train.p2p<-temp[["train"]]
test.p2p<-temp[["test"]]
train.mat <- sparse.model.matrix(def~., data = train.p2p)
train.l<-train.p2p[,"def"]%>%as.character()%>%as.numeric()
test.mat <- sparse.model.matrix(def~., data = test.p2p)
test.l<-test.p2p[,"def"]%>%as.character()%>%as.numeric()

##############################################优化
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- ks(preds,labels)
  return(list(metric = "ks", value = err))
}
dtrain <- xgb.DMatrix(data =train.mat,label = train.l)
dtest <- xgb.DMatrix(data =test.mat,label = test.l)
watchlist <- list(eval = dtest, train = dtrain)
param <- list(max.depth =3, eta = 0.07, silent = 0,gama=100,
              objective="binary:logistic",subsample=0.7,colsample_bytree=0.5,
              eval_metric=evalerror)
bst <- xgb.train(param, dtrain, nround =1000,watchlist=watchlist,maximize=TRUE)
pred <- predict(bst,test.mat)
ks(pred,test.l,plots = T)

