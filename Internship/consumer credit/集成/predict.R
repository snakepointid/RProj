library(stringr)
library(dplyr)
library(xgboost)
library(Matrix)
################################首先加载数据
################################file.dir为csv文件所在文件夹地址，
################################比如file.dir="C:/Users/uij/Desktop/datasets/variable type.csv"
data.ac<- read.csv("C:/Users/uij/Documents/R/my project/consumer credit/rawdatas/test_ac.csv", stringsAsFactors=FALSE)
data.beha<- read.csv("C:/Users/uij/Documents/R/my project/consumer credit/rawdatas/test_beha.csv", stringsAsFactors=FALSE)
vars<-names(data.ac)
data.ac<-filter(data.ac,flag_accountChangeMonth==1)
data.beha<-filter(data.beha,auth_id==1|auth_id==0)

################################################################对变量名进行改变
################################
for(i in 1:length(vars)){
  comp<-unlist(str_extract_all(vars[i],"[a-z,A-Z,0-9]+"))
  comp[comp=='acm']<-'ac'
  vars[i]<-cat_var(comp)
}

vars[vars=='ac_card_index']<-'card_index'
names(data.ac)<-vars



################################################################
for(i in names(data.ac)){
  comp<-unlist(str_extract_all(i,"[a-z,A-Z,0-9]+"))
  if('ac'%in%comp&!'card'%in%comp){
    print(i)
    comp<-c('I',comp)
    deri.var<-cat_var(comp)
    data.ac[,deri.var]<-get_deri(deri.var,data.ac)
  }
}


deri.ac<-vars.ac[!vars.ac%in%names(data.ac)]
deri.beha<-vars.beha[!vars.beha%in%names(data.beha)]
#############################################################
for(i in deri.ac){
  print(i)
  data.ac[,i]<-get_deri(i,data.ac)
}

for(i in deri.beha){
  print(i)
  data.beha[,i]<-get_deri(i,data.beha)
}
############################################################
data.ac[,vars.ac]<-define_types_by_predoc(data.ac[,vars.ac],variable.type)
data.beha[,vars.beha]<-define_types_by_predoc(data.beha[,vars.beha],variable.type)
data.ac<-miss_handle(data.ac,-9999)
data.beha<-miss_handle(data.beha,-9999)
############################################################
m.v<--9999
prob<-xg_predict(data.beha,vars.beha,bst.beha)
data.beha[,"beha_def"]<-prob

prob<-xg_predict(data.ac,vars.ac,bst.ac)
data.ac[,"ac_def"]<-prob
############################################################
ensemble.df<-full_join(data.beha[,c('beha_def','cus_num','id','cell','email','apply_date')],
                       data.ac[,c('ac_def','cus_num','id','cell','email','apply_date')],by=c("cus_num"))
ensemble.df[,'id']<-ifelse(is.na(ensemble.df[,"id.y"]),ensemble.df[,"id.x"],ensemble.df[,"id.y"])
ensemble.df[,'cell']<-ifelse(is.na(ensemble.df[,"cell.y"]),ensemble.df[,"cell.x"],ensemble.df[,"cell.y"])
ensemble.df[,'email']<-ifelse(is.na(ensemble.df[,"email.y"]),ensemble.df[,"email.x"],ensemble.df[,"email.y"])
ensemble.df[,'apply_date']<-ifelse(is.na(ensemble.df[,"apply_date.y"]),ensemble.df[,"apply_date.x"],ensemble.df[,"apply_date.y"])
ensemble.df<-ensemble.df[,c('ac_def','beha_def','id','cell','email','apply_date','cus_num')]

mp<-which(is.na(ensemble.df[,"ac_def"]))
ensemble.df[mp,"ac_def"]<-ac_miss
mp<-which(is.na(ensemble.df[,"beha_def"]))
ensemble.df[mp,"beha_def"]<-beha_miss
############################################################
probs<-predict(logic.model,newdata=ensemble.df,type="response")
ensemble.df[,'p_1']<-probs
score<-round(361.1196211-log(ensemble.df[,'p_1']/(1-ensemble.df[,'p_1']))*72.13475204)
score[score<300]<-300
score[score>1000]<-1000
ensemble.df[,'score']<-score
outcome<-ensemble.df[,c('id','cell','email','apply_date','cus_num','p_1','score')]