library(s.PP)
library(s.FS)
library(s.ME)
library(s.FE)
beha.df<-rawbeha[,c(7,8,18:628)]
ac.df<-rawac[,c(7,8,14:425)]

train.df<-full_join(beha.df,ac.df,by=c("cus_num"))
train.df[,'def']<-ifelse(is.na(train.df[,"flag1.y"]),train.df[,"flag1.x"],train.df[,"flag1.y"])
train.df[,'sex']<-ifelse(is.na(train.df[,"sex.y"]),train.df[,"sex.x"],train.df[,"sex.y"])
train.df[,'age']<-ifelse(is.na(train.df[,"age.y"]),train.df[,"age.x"],train.df[,"age.y"])
train.df<-select(train.df,-contains('.y'))
train.df<-select(train.df,-contains('.x'))
train.df<-select(train.df,-contains('m7m9'))
train.df<-select(train.df,-contains('m10m12'))
train.df<-select(train.df,-contains('m13m15'))
train.df<-select(train.df,-contains('m16m18'))
train.df<-select(train.df,-c(max_cons_m3_v_cate,max_cons_m6_v_cate,
                             max_cons_m12_v_cate,max_cons_m3_numcate, 
                             max_cons_m12_numcate,max_cons_m3_paycate,max_cons_m6_paycate, 
                             max_cons_m12_paycate,max_cons_m12_mp_cate))
train.df<-select(train.df,-cus_num)
####################################先去除完全没有的变量
names(temp)
vars<-names(train.df)
for(i in 1:length(vars)){
  comp<-unlist(str_extract_all(vars[i],"[a-z,A-Z,0-9]+"))
  comp[comp=='acm']<-'ac'
  vars[i]<-cat_var(comp)
}

vars[vars=='ac_card_index']<-'card_index'
names(train.df)<-vars
#####################################################
train.df<-define_types_by_predoc(train.df,variable.type)
train.df.m<-trans2miss(train.df,-1,'missing')
train.df<-miss_handle(train.df.m,-9999)
####################################先去除完全没有的变量
miss<-single_value_count(train.df,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
useful<-unique(c(useful,"def"))
train.df<-train.df[,useful]
###################################extract out miss info
useful<-rpart_filter(train.df, "def", controls = rpart.control(minsplit = 20, cp =0,
                                                               maxcompete = 1, maxsurrogate = 1, usesurrogate = 1, xval = 1, surrogatestyle =
                                                                 1, maxdepth = 12))
##############################################
library(shiny)
runApp('C:/Users/uij/Documents/R/my project/guangda rules/ensemble')
                                                       
############################################################################################
############################################################################################
############################################################################################
############################################################################################
set.seed(123)
tree<-rpart(def~.,train.df[,c(useful,'def')],
            control=rpart.control(minsplit = 100, maxdepth = 10, cp = 0, 
                                  maxcompete = 1, maxsurrogate = 0, usesurrogate = 0, xval =4,
                                  surrogatestyle = 0))


###########################################################
temp<-filter(train.df,tot_consmedia_m6_catenum< 4.5,
             card_index< 3.5,
             ac_m1m6_debit_balance_tr%in%c(0,1,2,4),
             card_index< 2.5,
             sex=="male",
             ac_m10m12_debit_balance< 68.5,
             time_recent_dout6_max%in%c(1,2,3),
             ac_num13_debit_in< 2.5,
             age< 40.5,
             ac_sum16_debit_balance< 375)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
###########################################################
temp<-filter(train.df,tot_consmedia_m6_catenum< 4.5,
             ac_m1m6_debit_balance_tr%in%c(0,1,2,4),
             card_index< 2.5,
             sex=='male',
             ac_m1m6_debit_out_3w==0,
             ac_sum112_debit_balance< 375,
             age< 41.5)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])
###########################################################
temp<-filter(train.df,tot_consmedia_m6_catenum>=4.5,
             max_cons_m6_numcate%in%c('C11','C12','C23','C25','C26','C28','C29'))
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])

###########################################################
temp<-filter(train.df.m,
             sex=='male',
             card_index< 2.5,
             ac_avg112_debit_balance< 531.2)
table(temp[,'def'])%>%prop.table()
length(temp[,"def"])

