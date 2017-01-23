library(dplyr)
library(parallel)
library(C50)
##########################################查看取值范围

p<-which(rawdata[,"other_var02"]==9)
p2p<-rawdata[-p,]
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
miss<-missing_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]
temp<-count_distinct_value(p2p,names(p2p))
##################################################定义类型
d.l<-c("assets_wealth","auth_key_relation")
p2p<-naive_defi_type(p2p,d.l,3)
str(p2p)
missp2p<-miss_valued(p2p,"def")
####################################计算预测力统计量
temp<-sepe_var(missp2p,"def")
dis.var<-temp[[1]]
con.var<-temp[[2]]
iv.dis<-get_df_iv(missp2p,dis.var,"def")
iv.con<-get_df_iv(missp2p,con.var,"def")

use.dis<-names(iv.dis[iv.dis>0.02])
use.con<-names(iv.con[iv.dis>0.02])

useful<-c(use.dis,use.con)

############################################################开始建模
set.seed(1223)
cv.list<-cv_balance(missp2p,4,targ="def")

###################################################model
missp2p<-miss_handle(missp2p)

test<-list()
test[[1]]<-useful
for(i in 2:100){
  temp<-test[[i-1]]
  test[[i]]<-variable_select_bw(para_C50,missp2p,temp,0.003,controls.C50)[[1]]
  paste("这是第",i,"次循环了")
}
test5<-test[[3]]
library(rpart)
library(rattle)
rpart<-rpart(def~.,data=missp2p[,c(test4,"def")],control = controls.rpart)
asRules(rpart)


missp2p<-outlier_handle(missp2p)
library(adabag)
boost<-boosting(def~.,data=missp2p[-cv.list[[1]],c(test5,"def")], boos = TRUE, mfinal = 100 
                ,control=controls.rpart)
pred<-predict(boost,newdata = missp2p[cv.list[[1]],c(test5,"def")])
ks(pred$prob[,2],missp2p[cv.list[[1]],"def"],plots = T)

