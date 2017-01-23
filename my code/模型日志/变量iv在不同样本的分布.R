library(dplyr)
library(parallel)
library(C50)
library(rpart)
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

############################################################对分类和连续变量进行异常值和缺失值处理
p2p.model<-miss_handle(p2p,num.handle=T)
set.seed(12323)
cv.list<-cv_balance(p2p.model,4,targ="def")
############################################################开始建模
df<-c()
test.vars<-names(p2p)
for(i in 1:4){
temp<-p2p.model[cv.list[[i]],]
temp<-get_var_info(temp,test.vars,"def",get_IV)
iv<-temp[order(names(temp))]
df<-cbind(df,iv)
}
write.csv(df,file="list.csv")