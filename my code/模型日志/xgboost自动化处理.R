library(dplyr)
library(s.PP)
#library(FS)
##########################################查看取值范围
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

miss<-missing_count(p2p)
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,useful]

miss<-single.value_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]

############################################################定义类型
temp<-count_distinct_value(p2p,names(p2p))
d.l<-"auth_key_relation"
p2p<-define_types(p2p,d.l,3)
str(p2p)

############################################################对分类和连续变量进行异常值和缺失值处理
p2p.model<-miss_handle(p2p,num.handle=T)
#p2p.model<-robust_df(p2p.model,"def",500)
#p2p.model<-multivars_analysis(p2p.model,get_IV,"def",10)
set.seed(12323)
cv.list<-cv_balance(p2p.model,4,targ="def")


##############################################
df<-p2p.model
#############################################按iv值进行筛选
useful<-iv_sig_filter(df,"spearman",cor.rate=0.7,iv.level=0.02,sig.level=0.05)
useful<-sample(useful,length(useful),replace = F)
IV_sig_FS<-variable_select_xgboost(df,useful,para,300,0.003,10)
##############################################
df<-p2p.model
rpart<-rpart(def~.,data=df,control=controls.rpart)
useful<-names(rpart$variable.importance)
useful<-sample(useful,length(useful),replace = F)
rpart_FS<-variable_select_xgboost(df,useful,para,300,0.003,10)
##############################################
df<-p2p.model
useful<-names(df)
useful<-useful[useful!="def"]
useful<-sample(useful,length(useful),replace = F)
all_FS<-variable_select_xgboost(df,useful,para,200,0.003,10)
