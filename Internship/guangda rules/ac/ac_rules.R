library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
train.df<-rawac[,c(8,14:425)]
train.df<-rename(train.df,def=flag1)
train.df<-select(train.df,-contains('m7m9'))
train.df<-select(train.df,-contains('m10m12'))
train.df<-select(train.df,-contains('m13m15'))
train.df<-select(train.df,-contains('m16m18'))
train.df<-select(train.df,-contains('debit_use'))
train.df<-select(train.df,-contains('credit_use'))
train.df<-select(train.df,-matches("_m[1-6]_"))
train.df<-trans2miss(train.df,-1,"missing")
####################################################
vars<-names(train.df)
for(i in 1:length(vars)){
  comp<-unlist(str_extract_all(vars[i],"[a-z,A-Z,0-9]+"))
  comp[comp=='acm']<-'ac'
  vars[i]<-cat_var(comp)
}

vars[vars=='ac_card_index']<-'card_index'
names(train.df)<-vars
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
useful<-rpart_filter(train.df, "def", controls = 
                       rpart.control(minsplit = 5, cp =0,maxcompete = 1, maxsurrogate = 1, 
                                     usesurrogate = 1, xval = 1, surrogatestyle =1, 
                                     maxdepth = 12))
##############################################
library(shiny)
runApp('C:/Users/uij/Documents/R/my project/guangda rules/ensemble')

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
table(train.df[,'def'])%>%prop.table()
temp<-filter(train.df,card_index<= 2,
             sex%in%c('female','unknown'),
             ac_sum112_debit_balance< 600,
             ac_sum112_debit_balance>0)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
temp<-filter(train.df,
             sex=='male',
             card_index<=1,
             ac_sum112_debit_balance< 6000,
             ac_sum112_debit_balance>0)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
temp<-filter(train.df,
             sex%in%c('female','unknown'),
             card_index<= 2,
             ac_sum112_debit_balance< 600,
             ac_sum112_debit_balance>0,
             ac_m1m6_debit_out_tr%in%c(4,5))
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
############################################################################################
temp<-filter(train.df,
             ac_sum112_debit_balance< 1000,
             ac_sum112_debit_balance>0,
             card_use_type12%in%c(2,4),
             age< 40,
             ac_mean13_debit_out< 550,
             ac_mean13_debit_out>0)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])