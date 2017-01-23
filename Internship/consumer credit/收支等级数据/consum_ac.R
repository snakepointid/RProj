library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
train.df<-rawdata[,c(18,135,137:445)]
train.df<-rename(train.df,def=default)
train.df<-trans2miss(train.df,-1,"missing")
########################################删除标示变量
#可删除变量"flag","other","date"系列id,opendid,mail
####################################先去除完全没有的变量
pre.types<-variable.type[,-2]
train.df<-define_types_by_predoc(train.df,pre.types)
train.df<-miss_handle(train.df,-1)

###################no strange sign
###################define missing value
####################################先去除完全没有的变量
miss<-single_value_count(train.df,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
useful<-unique(c(useful,"def"))
train.df<-train.df[,useful]

##########################get types
temp<-get_types(train.df,"def")
dis.vars<-temp[[1]]
con.vars<-temp[[2]]
all.vars<-c(dis.vars,con.vars)
###################################extract out miss info
train.df.m<-define_types(train.df,all.vars)
train.df.m<-trans2miss(train.df.m,"-1","missing")

train.df<-extra_out_miss(train.df,train.df.m,con.vars,"con_outer_count",out=T)
train.df<-extra_out_miss(train.df,train.df.m,con.vars,"con_misser_count",out=F)
train.df<-extra_out_miss(train.df,train.df.m,dis.vars,"dis_outer_count",out=T)
train.df<-extra_out_miss(train.df,train.df.m,dis.vars,"dis_misser_count",out=F)
train.df<-extra_out_miss(train.df,train.df.m,all.vars,"all_outer_count",out=T)
train.df<-extra_out_miss(train.df,train.df.m,all.vars,"all_misser_count",out=F)
#######################################standerize
temp<-get_types(train.df,"def")
dis.vars<-temp[[1]]
con.vars<-temp[[2]]
all.vars<-c(dis.vars,con.vars)
st.fit<-stand_fit(train.df,con.vars,"stm")
train.df<-stand_transform(train.df,st.fit)

############################################################进行单变量筛选
iv.info<-get_df_info(df =train.df,all.vars,targ='def',lowest=500,way="iv")
############################################################进行单变量筛选
useful<-iv_filter(df=train.df,targ="def", iv.info=iv.info, lowest = 500, method = "spearman",
                  cor.rate = 0.8, dis.level = 0.01, con.level = 0.01)
############################################################进行单变量筛选
names(rawdata)
FS<-SBS(train.df,useful,"def",0,para_logic,scorer =ks,nfolds = 4,info=iv.info,aug=T)
sbs_curve(FS)
length(FS[[1]])
FS[[3]][[59]]
