library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
train.df<-rawbeha[,c(8,18:628)]
train.df<-rename(train.df,def=flag1)
train.df<-trans2miss(train.df,-1,"missing")
train.df<-select(train.df,-c(max_cons_m3_v_cate,max_cons_m6_v_cate,
                             max_cons_m12_v_cate,max_cons_m3_numcate, 
                             max_cons_m12_numcate,max_cons_m3_paycate,max_cons_m6_paycate, 
                             max_cons_m12_paycate,max_cons_m12_mp_cate))

####################################################
####################################先去除完全没有的变量
train.df<-define_types_by_predoc(train.df,variable.type)
train.df.m<-trans2miss(train.df,-1,'missing')
train.df<-miss_handle(train.df.m,-9999)
###################no strange sign
###################define missing value
####################################先去除完全没有的变量
miss<-single_value_count(train.df,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
useful<-unique(c(useful,"def"))
train.df<-train.df[,useful]
###################################extract out miss info
useful<-rpart_filter(train.df, "def", controls = rpart.control(minsplit = 5, cp =0,
                                                               maxcompete = 1, maxsurrogate = 1, usesurrogate = 1, xval = 1, surrogatestyle =
                                                                 1, maxdepth = 12))
##############################################
library(shiny)
runApp('C:/Users/uij/Documents/R/my project/guangda rules/ensemble')
table(train.df[,'def'])%>%prop.table()
############################################################################################
############################################################################################
############################################################################################
############################################################################################
temp<-filter(train.df,
             tot_cons_m12_pay<=600,
             auth_name==1,
             max_cons_m6_numcate%in%c('C11','C14','C15','C16','C19','C23','C25','C7','C88'),
             cons_m3_X_visits>=6)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
temp<-filter(train.df,
             tot_consmedia_m6_catenum<=4,
             auth_name==0,
             sex=='male',
             max_cons_m6_numcate%in%c('C10','C16','C23','C4'))
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
temp<-filter(train.df,
             tot_cons_m12_pay>=600,
             cons_m6_WLYXXNWP_num>=1500)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
table(train.df[,"def"])%>%prop.table()

############################################################################################
