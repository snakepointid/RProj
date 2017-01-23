library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
##########################
data_beha<-data.frame(raw_beha[,-7],row.names =raw_beha[,7])
data_beha<-rename(data_beha,default=flag1)
data_beha<-select(data_beha,-contains("level"))
data_beha<-select(data_beha,-contains("flag"))
useful<-names(data_beha)
useless<-c('openid','id','cell','email','name','home_tel',
           'apply_date','observe_date','swift_number',
           'branch_name',"Contact_Rela_Type_Cd","Industry_Class_Cd","Corp_Kind_Cd"              
           ,"Annual_Revenue","Gender_cd","Marriage_Stat_cd","House_Type_cd"             
           ,"Education_level_cd","Headship_cd","Metier_Level_cd","Subbranch_Cd"              
           ,"islocal","gender_islocal",'credit_use12','debit_use12','credit_use6','debit_use6'
           ,'credit_use3','debit_use3')
useful<-useful[!useful%in%useless]
alldata<-data_beha[,useful]
########################################################
set.seed(1)
test_num<-sample(1:length(alldata[,1]),3000,replace = F)
target_y<-alldata[test_num,'default'] 
alldata[test_num,'default']<-NA
########################################################
########################################################
find_strange_sign(alldata,100)
alldata<-stran_sign_handle(alldata,'-1')
########################################################
cat.vars<-c("max_media_m3_cate","max_media_m6_cate"
            ,"max_media_m12_cate","max_cons_m3_v_cate"
            ,"max_cons_m6_v_cate","max_cons_m12_v_cate"
            ,"max_cons_m3_numcate","max_cons_m6_numcate"
            ,"max_cons_m12_numcate","max_cons_m3_paycate"
            ,"max_cons_m6_paycate","max_cons_m12_paycate"
            ,"max_cons_m12_mp_cate")
for(i in cat.vars){
  alldata[,i]<-as.factor(as.character(alldata[,i]))
}
useful<-useful[!useful%in%cat.vars]
########################################################
alldata[,useful]<-define_types_by_predoc(alldata[,useful],predoc)
train<-which(!is.na(alldata[,'default']))
alldata<-miss_handle(alldata,-9999)
alldata<-one_hot_transform(alldata,'default')
traindata<-alldata[train,]
validata<-alldata[test_num,]
############################################################
useful<-rpart_filter(traindata,"default",controls=rpart.control(minsplit = 30,
                                                                maxdepth = 12, cp = 0, maxcompete = 1,
                                                                maxsurrogate = 1, usesurrogate = 1, 
                                                                xval =4,surrogatestyle = 0)) 
#######################################################
############################################################
m.v<--9999
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- ks(preds,labels)
  return(list(metric = "scoring", value = err))
}
best.para<-beha_simple_para
##################################################################
train.mat <- do.call(cbind,traindata[,useful])
train.l<-traindata[,'default']%>%as.character()%>%as.numeric()
dtrain <- xgb.DMatrix(data =train.mat,label = train.l,missing = m.v)

vali.mat <- do.call(cbind,validata[,useful])
vali.l<-target_y%>%as.character()%>%as.numeric()
dvali <- xgb.DMatrix(data =vali.mat,label = vali.l,missing = m.v)
###############################
# set.seed(123)
# bsts <- xgb.cv(params=best.para, data=dtrain,nround =1000,verbose = T,nfold=4)
# ################################
set.seed(123)
bst <- xgb.train(params=best.para, data=dtrain,nround =100, verbose = 1,watchlist=list('train'= dtrain,'validation'=dvali))
var.imp<-xgb.importance(useful, model = bst)
var.imp$Feature