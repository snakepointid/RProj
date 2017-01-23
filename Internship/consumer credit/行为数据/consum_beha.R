library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
p2p<-trans2miss(rawdata,-1,"missing")
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
p2p<-select(p2p,-contains("mail"))
p2p<-select(p2p,-contains("bank"))
p2p<-select(p2p,-c(id,openid,cell,name,MatchType,auth_key_relation,auth_home_tel,auth_biz_tel,stab_cell_firsttime))
####################################先去除完全没有的变量
pre.types<-variable.type[,-2]
p2p<-define_types_by_predoc(p2p,pre.types)
p2p<-miss_handle(p2p,-1)

###################no strange sign
###################define missing value
####################################先去除完全没有的变量
miss<-single_value_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
useful<-unique(c(useful,"def"))
p2p<-p2p[,useful]

##########################get types
temp<-get_types(p2p,"def")
dis.vars<-temp[[1]]
con.vars<-temp[[2]]
all.vars<-c(dis.vars,con.vars)
###################################extract out miss info
p2p.m<-define_types(p2p,all.vars)
p2p.m<-trans2miss(p2p.m,"-1","missing")

p2p<-extra_out_miss(p2p,p2p.m,con.vars,"con_outer_count",out=T)
p2p<-extra_out_miss(p2p,p2p.m,con.vars,"con_misser_count",out=F)
p2p<-extra_out_miss(p2p,p2p.m,dis.vars,"dis_outer_count",out=T)
p2p<-extra_out_miss(p2p,p2p.m,all.vars,"all_outer_count",out=T)
p2p<-extra_out_miss(p2p,p2p.m,all.vars,"all_misser_count",out=F)
#######################################standerize
temp<-get_types(p2p,"def")
dis.vars<-temp[[1]]
con.vars<-temp[[2]]
all.vars<-c(dis.vars,con.vars)
st.fit<-stand_fit(p2p,con.vars,"stm")
p2p<-stand_transform(p2p,st.fit)
p2p<-traindf_outlier_handle(p2p,0.01)
############################################################进行单变量筛选
iv.info<-get_df_info(df =p2p,all.vars,targ='def',lowest=500,way="iv")
############################################################进行单变量筛选
useful<-rpart_filter(p2p,"def")
############################################################进行单变量筛选
FS<-SBS(p2p,useful,"def",0,para_logic,scorer =ks,nfolds = 4,info=iv.info,aug=T)
sbs_curve(FS)
length(FS[[1]])
FS[[3]][[59]]
rpart_filter
