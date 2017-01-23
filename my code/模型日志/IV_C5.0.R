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
useful<-muti_value(p2p,"def")
p2p<-p2p[,c(useful,"def")]

miss<-missing_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]
str(p2p)

############################################################定义类型
temp<-count_distinct_value(p2p,names(p2p))
d.l<-names(p2p)[c(98:110)]
v.l<-names(p2p)[-c(98:110)]
p2p<-naive_defi_type(p2p,v.l,d.l)
p2p<-miss_valued(p2p,"def")
####################################进行变量筛选
temp<-outlier_count(p2p)
p2p[,"outer_con"]<-temp[[1]]%>%as.numeric()
p2p[,"outer_dis"]<-temp[[2]]%>%as.numeric()
p2p[,"misser_flag"]<-misser_count(p2p)%>%as.numeric()

####################################计算预测力统计量
temp<-sepe_var(p2p,"def")
dis.var<-temp[[1]]
con.var<-temp[[2]]

iv.con<-get_df_iv(p2p,con.var,"def")
iv.dis<-get_df_iv(p2p,dis.var,"def")
######################选择变量
use.dis<-names(iv.dis)[order(iv.dis,decreasing = T)]
use.dis<-use.dis[1:40]
use.con<-names(iv.con)[order(iv.con,decreasing = T)]
use.con<-use.con[1:80]
useful<-c(use.dis,use.con)

info.matr<-info_matr(useful,p2p)
useful<-keep.or.drop(info.matr,50)
############################################################对分类和连续变量进行异常值和缺失值处理
p2p.model<-p2p
############################################################开始建模
set.seed(123)
cv.list<-cross_validation(p2p.model,4)
cv.list<-cv_balance(p2p.model,4,targ="def")
###################################################model
test<-variable_select_bw(p2p.model,useful,0.003)
#######################################model
temp<-para_boost(p2p.model,test[[1]],cv.list,ks,control=controls)
