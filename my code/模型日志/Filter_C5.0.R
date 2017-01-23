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
d.l<-"def"
p2p<-naive_defi_type(p2p,d.l,3)
str(p2p)
####################################进行变量筛选

####################################计算预测力统计量
temp<-sepe_var(p2p,"def")
dis.var<-temp[[1]]
con.var<-temp[[2]]

temp<-outlier_count(p2p)
p2p[,"outer_con"]<-temp[[1]]%>%as.numeric()
p2p[,"outer_dis"]<-temp[[2]]%>%as.numeric()
p2p[,"misser_flag"]<-misser_count(p2p)%>%as.numeric()

iv.con<-get_df_iv(p2p,con.var,"def")
iv.dis<-get_df_iv(p2p,dis.var,"def")
iv.dis<-iv.dis[iv.dis<0.01]
iv.dis["is_cons_m3_JJQC_num"]
get_df_iv(p2p,"new","def")
new.iv<-c()
for(i in names(iv.dis)){
  for(j in names(iv.dis)){
  p2p[,"new"]<-paste(p2p[,j],p2p[,i])
  temp<-get_df_iv(p2p,"new","def")
  temp%>%print()
  names(temp)<-paste(i,"和",j)
  new.iv<-c(new.iv,temp)
  print(paste(i,"和",j,"组成新变量"))
  }
}
new.iv[order(new.iv)]
p2p[,"new"]<-paste(p2p[,"is_cons_m3_JJQC_num"],p2p[,"is_cons_m12_JJQC_num"])
p2p[,"new"]%>%table()
######################选择变量
use.dis<-names(iv.dis)[order(iv.dis,decreasing = T)]
use.dis<-use.dis[1:40]
use.con<-names(iv.con)[order(iv.con,decreasing = T)]
use.con<-use.con[1:80]
useful<-unique(c(use.dis,use.con,"outer_con","outer_dis","misser_flag"))
############################################################对分类和连续变量进行异常值和缺失值处理
p2p.model<-p2p
p2p.model<-outlier_handle(p2p.model,use.con)
############################################################开始建模
set.seed(123)
#cv.list<-cross_validation(p2p.model,4)
cv.list<-cv_balance(p2p.model,4,targ="def")
###################################################model

test<-variable_select_bw(p2p.model,useful,0.003)
#######################################model
#temp<-para_boost(p2p.model,test[[1]],cv.list,ks,control=controls)
library(gbm)
gbm(def~.,data=p2p.model, distribution = "adaboost",n.trees = 100,
    interaction.depth = 2)
