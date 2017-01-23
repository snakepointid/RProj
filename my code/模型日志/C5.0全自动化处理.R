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
p2p<-select(p2p,-c(id,openid,mail,cell,name))

############################################################定义类型
temp<-var.type[,1]%in%c("auth_stab_id","auth_stab_cell","auth_stab_mail")
var.type[temp,2]<-c("分类变量")
temp<-define_type(p2p,var.type)
p2p<-temp[[1]]
v.l<-temp[[2]]
temp<-count_distinct_value(p2p,v.l)
p2p<-naive_defi_type(p2p,v.l)
####################################进行变量筛选
####################################先去除完全没有的变量
useful<-muti_value(p2p,"def")
p2p<-p2p[,c(useful,"def")]

miss<-missing_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]
str(p2p)
####################################计算预测力统计量
temp<-sepe_var(p2p,"def")
dis.var<-temp[[1]]
con.var<-temp[[2]]

iv.con<-get_df_iv(p2p,c(con.var,"def"),"def")
iv.dis<-get_df_iv(p2p,c(dis.var,"def"),"def")
######################选择变量
use.con<-names(iv.con)[which(iv.con>0.06)]
use.dis<-names(iv.dis)[which(iv.dis>0.02)]
useless<-cor_test(p2p,use.con,0.75,iv.con)
use.con<-use.con[!use.con%in%useless]
############################################################对分类和连续变量进行异常值和缺失值处理
p2p.model<-p2p
p2p.model<-miss_handle(p2p.model,c(use.con,use.dis))
############################################################开始建模
#p2p.balance<-balance_df_cut(p2p.model,"def")
cv.list<-cross_validation(p2p.model,8)
#cv.list.bal<-cross_validation(p2p.balance,12)
###################################################model
#model.dis<-para_boost(p2p.model,use.dis,cv.list,ks,control=controls)
#model.con<-para_boost(p2p.model,use.con,cv.list,ks,control=controls)
#model.all<-para_boost(p2p.model,c(use.con,use.dis),cv.list,ks,control=controls)
#mean(model.all)


use.dis<-names(iv.dis[use.dis])[order(iv.dis[use.dis],decreasing = T)]
for(i in 37:1){
  temp<-para_boost(p2p.model,use.dis[c(1:22,38)],cv.list,ks,control=controls)
  mean(temp)%>%print()
  print(i)
  }
use.dis[38]

