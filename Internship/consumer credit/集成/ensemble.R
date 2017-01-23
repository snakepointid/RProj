library(s.FE)
library(s.ME)
library(s.FS)
library(s.PP)
data.ac<-rawac
data.beha<-rawbeha
data.ac<-rename(data.ac,def=default)
data.ac<-data.ac[,c("id","cell","def",vars.ac)]
data.beha<-data.beha[,c("id","cell","def",vars.beha)]
########################################################
data.ac[,c("def",vars.ac)]<-define_types_by_predoc(data.ac[,c("def",vars.ac)],variable.type)
data.beha[,c("def",vars.beha)]<-define_types_by_predoc(data.beha[,c("def",vars.beha)],variable.type)
########################################################
data.ac<-miss_handle(data.ac,-1)
data.beha<-miss_handle(data.beha,-1)
########################################################
set.seed(123)
test.position = balance_cut(data.ac,0.3,"def")
train.ac<-data.ac[-test.position,]
test.ac<-data.ac[test.position,]

test.position = balance_cut(data.beha,0.3,"def")
train.beha<-data.beha[-test.position,]
test.beha<-data.beha[test.position,]
########################################################
set.seed(123)
m.v=-1
beha.para<-list(max.depth =8,scale_pos_weight=0.1,lambda=1,gamma=1,colsample_bytree=0.8,
                min_child_weight=1,subsample=0.6,eta = 0.07,objective="binary:logistic",nthread=4)
trees<-1000
bst<-xg_train(train.beha, vars.beha, "def", nround = trees, para = beha.para)
prob<-xg_predict(test.beha,vars.beha,bst)
test.beha[,"beha_def"]<-prob

ac.para<-list(max.depth =3,scale_pos_weight=0.1,lambda=10,gamma=0.07,colsample_bytree=0.8,
              min_child_weight=1,subsample=1,eta = 0.07,objective="binary:logistic",nthread=4)
trees<-1000
bst<-xg_train(train.ac, vars.ac, "def", nround = trees, para = ac.para)
prob<-xg_predict(test.ac,vars.ac,bst)
test.ac[,"ac_def"]<-prob
########################################################

test.beha<-test.beha[,c("def","beha_def","id","cell")]
test.ac<-test.ac[,c("def","ac_def","id","cell")]
ensemble.df<-full_join(test.beha,test.ac,by=c("id","cell"))
mp<-which(is.na(ensemble.df[,"def.x"]))
ensemble.df[mp,"def.x"]<-ensemble.df[mp,"def.y"]
ensemble.df<-ensemble.df[,c("def.x","ac_def","beha_def")]
ensemble.df<-rename(ensemble.df,def=def.x)
########################################################sulo1
impute_miss<-function(vec,target){
  cut.point<-quantile(vec,seq(0,1,0.05),na.rm = T)
  new.vec<-cut(vec,unique(cut.point))%>%as.character()
  miss.df<-ensemble.df[is.na(new.vec),"def"]%>%table()%>%prop.table()
  miss.rate<-miss.df[1]
  temp<-table(new.vec,target)%>%prop.table(.,1)
  b<-abs(temp[,1]-miss.rate)%>%which.min()%>%names()%>%print()
  b<-unlist(str_extract_all(b,"\\.\\d+"))%>%as.numeric()
  return(mean(b))
}
ac_miss<-impute_miss(ensemble.df[,"ac_def"],ensemble.df[,"def"])
beha_miss<-impute_miss(ensemble.df[,"beha_def"],ensemble.df[,"def"])

mp<-which(is.na(ensemble.df[,"ac_def"]))
ensemble.df[mp,"ac_def"]<-ac_miss
mp<-which(is.na(ensemble.df[,"beha_def"]))
ensemble.df[mp,"beha_def"]<-beha_miss
########################################################
para_logic(ensemble.df,c("ac_def","beha_def"),"def",ks,nfolds=4)
logic.model<-glm(def~.,data=ensemble.df,family =binomial(link = "logit"))
########################################################
trees<-1000
bst.beha<-xg_train(data.beha, vars.beha, "def", nround = trees, para = beha.para)
##########
trees<-1000
bst.ac<-xg_train(data.ac, vars.ac, "def", nround = trees, para = ac.para)
##########################################################################

