library(dplyr)
inner_p2p<-inner_join(beha,check,by=c("id","cell"))
left_p2p<-left_join(beha,check,by=c("id","cell"))
full_p2p<-full_join(beha,check,by=c("id","cell"))
##########################################建模
p2p<-inner_p2p
p2p<-select(p2p,-contains("date"))
p2p<-select(p2p,-contains("other"))
p2p<-select(p2p,-contains("brand"))
p2p<-select(p2p,-contains("score"))
p2p<-select(p2p,-contains("level"))
p2p<-select(p2p,-starts_with("flag"))
p2p<-p2p[,-c(1:5)]
p2p<-trans2miss(p2p,-1)
#temp<-select(p2p,-starts_with("cons"))
#temp<-select(temp,-starts_with("media"))
#temp<-select(temp,-ends_with("cate"))
p2p<-p2p[,names(temp)]
#######################################缺失和单值处理
miss<-missing_count(p2p,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
p2p<-p2p[,c(useful,"def")]
temp<-count_distinct_value(p2p,names(p2p))
##################################################定义类型
d.l<-c("auth_key_relation")
p2p<-naive_defi_type(p2p,d.l,3)
str(p2p)
missp2p<-miss_valued(p2p,"def")


############################################################开始建模
set.seed(1223)
missp2p<-miss_handle(missp2p)
#missp2p<-filter(missp2p,name_flag=="N")
cv.list<-cv_balance(missp2p,4,targ="def")

###################################################model

test<-list()
test[[1]]<-useful
for(i in 2:100){
  temp<-test[[i-1]]
  test[[i]]<-variable_select_bw(para_rpart,missp2p,temp,0.003,controls.rpart)[[1]]
  paste("这是第",i,"次循环了")
}
test5<-test[[15]]
test5<-c("max_cons_m3_paycate","auth_key_relation","tot_cons_m12_visits")
library(rpart)
library(rattle)
rpart<-rpart(def~.,data=missp2p[,c(useful,"def")],control = controls.rpart)
asRules(rpart)
test5
str(missp2p[,c(test5,"def")])
missp2p<-outlier_handle(missp2p)
library(adabag)
boost<-boosting(def~.,data=temp[-cv.list[[1]],], boos = TRUE, mfinal = 100 
                ,control=controls.rpart)
pred<-predict(boost,newdata = temp[cv.list[[1]],])
ks(pred$prob[,2],missp2p[cv.list[[1]],"def"],plots = T)


