library(s.PP)
library(s.FS)
library(s.ME)
p2p<-filter(rawdata,other_var02!=9&other_var02!=7)
p2p<-trans2miss(p2p,-1,"missing")


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

###################find strange sign
find_strange_sign(p2p,100)
###################no strange sign
###################define missing value
####################################先去除完全没有的变量
miss<-missing_count(p2p)
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,useful]

miss<-single_value_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
useful<-unique(c(useful,"def"))
p2p<-p2p[,useful]
############################################################定义类型
temp<-count_distinct_value(p2p,names(p2p))
d.l<-c("auth_key_relation","cell3")
p2p<-define_types(p2p,d.l,6)
str(p2p)
############################################################对分类和连续变量进行异常值和缺失值处理
cut.fit<-cut_fit(p2p,"def",500)
cut.p2p<-cut_transform(p2p,cut.fit)
p2p.model<-miss_handle(cut.p2p,-1)
w.f<-woe_fit(p2p.model,"def")
p2p.woe<-woe_transform(p2p.model,w.f)
############################################################进行单变量筛选
useful<-names(p2p.woe)
useful<-useful[useful!="def"]
FS<-SBS(p2p.woe,useful,"def",0.003,para_logic)
