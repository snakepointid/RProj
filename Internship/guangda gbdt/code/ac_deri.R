library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
##########################
data_ac<-data.frame(raw_ac[,-7],row.names =raw_ac[,7])
data_ac<-rename(data_ac,default=flag1)
data_ac<-select(data_ac,-contains("level"))
data_ac<-select(data_ac,-contains("flag"))
alldata<-data_ac
########################################################
set.seed(1)
test_num<-sample(1:length(alldata[,1]),3000,replace = F)
target_y<-alldata[test_num,'default'] 
alldata[test_num,'default']<-NA
########################################################
########################################################
find_strange_sign(alldata,100)
alldata<-stran_sign_handle(alldata,c('-1',''))
#########derived from original variable
for(i in c("matchloc","constellation","age","sex","phoneComp","mailComp",'phoneloc','birloc')){
  alldata[,i]<-get_deri(i,alldata)
}

###############################################
vars<-c('phoneloc','birloc')
for(i in vars){
  tempdoc<-promac_doc
  tempvars<-names(tempdoc)
  names(tempdoc)<-vapply(tempvars,function(s)paste(i,s,sep='_'),'s')
  tempdoc[,'iiid']<-tempdoc[,1]
  alldata[,'iiid']<-alldata[,i]
  alldata<-left_join(alldata,tempdoc[-1],by='iiid')
}
##################################mac info from city
alldata[,"iiid"]<-str_extract(alldata[,"id"],"\\d{1,4}")%>%as.numeric()
alldata<-left_join(alldata,id_city_mac_doc,by='iiid')
alldata[,"iiid"]<-str_extract(alldata[,"cell"],"\\d{1,7}")%>%as.numeric()
alldata<-left_join(alldata,phone_city_mac_doc,by='iiid')
################################################################
vars<-c('home_tel')
for(i in vars){
  alldata[,paste(i,'_rec',sep = "")]<-as.factor(as.numeric(is.na(alldata[,i])))
}
###############################################drop variables
allvars<-names(alldata)
useless<-c('openid','id','cell','email','name','home_tel',
           'apply_date','observe_date','swift_number',
           'branch_name',"Contact_Rela_Type_Cd","Industry_Class_Cd","Corp_Kind_Cd"              
           ,"Annual_Revenue","Gender_cd","Marriage_Stat_cd","House_Type_cd"             
           ,"Education_level_cd","Headship_cd","Metier_Level_cd","Subbranch_Cd"              
           ,"islocal","gender_islocal",'iiid','credit_use12','debit_use12','credit_use6','debit_use6'
           ,'credit_use3','debit_use3')
########
cat2num.vars<-c('phoneloc','birloc','constellation')
#############################################
useful<-allvars[!allvars%in%useless]
alldata<-alldata[,useful]
#############################################
alldata<-value2odds(alldata,cat2num.vars,'default',lowest=30,-9999)
useful<-useful[!useful%in%cat2num.vars]

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
best.para<-ac_deri_para
##################################################################
train.mat <- do.call(cbind,traindata[,useful])
train.l<-traindata[,'default']%>%as.character()%>%as.numeric()
dtrain <- xgb.DMatrix(data =train.mat,label = train.l,missing = m.v)

vali.mat <- do.call(cbind,validata[,useful])
vali.l<-target_y%>%as.character()%>%as.numeric()
dvali <- xgb.DMatrix(data =vali.mat,label = vali.l,missing = m.v)
# ################################
# set.seed(123)
# bsts <- xgb.cv(params=best.para, data=dtrain,nround =10000,verbose = T,nfold=4)
# ################################
set.seed(123)
bst <- xgb.train(params=best.para, data=dtrain,nround =100, verbose = 1,watchlist=list('train'= dtrain,'validation'=dvali))
var.imp<-xgb.importance(useful, model = bst)
var.imp$Feature