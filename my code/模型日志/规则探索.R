library(dplyr)
set.seed(25)
rawdata<-data.frame(gid1,gid2[,-c(1:4)])
gid<-rawdata
rm(gid1,gid2)
gid<-select(gid,-contains("time"))
gid<-data.frame(gid[,-c(1,3,4)])
##########################################清洗
find_strange_sign(gid,100)
v.l<-strange_meaning("",gid)
gid<-stran_sign_handle(v.l,"",gid)

useful<-muti_value(gid,"def")
gid<-gid[,c(useful,"def")]

miss<-missing_count(gid,"def")
wm<-which(miss<0.95)
useful<-names(miss)[wm]
gid<-gid[,c(useful,"def")]
############################################################定义类型
temp<-define_type(gid,var.type)
gid<-temp[[1]]
not.defi.var<-temp[[2]]
temp<-count_distinct_value(gid,not.defi.var)
vars<-temp[,1]
cate.df<-select(gid[,vars],contains("cate"))
dis.var<-names(cate.df)
navi.var<-vars[!vars%in%dis.var]
gid<-naive_defi_type(gid,navi.var)
for(i in dis.var){
  gid[,i]<-as.factor(gid[,i])
}
table(gid[,"def"])
460/(7045+460)
###########o##################################################
bala.gid.cut<-balance_df_cut(gid,"def")
bala.gid.add<-balance_df_add(gid,"def")
df<-bala.gid.add
temp<-random_cut(df,0.05)
triandf<-temp[["train"]]
testdf<-temp[["test"]]
#############################################################model
library(C50)
set.seed(4)
tree<-C5.0(def~.,data=df,rules=TRUE,control=C5.0Control(minCases =20,CF=0.5,sample=0.7))
#pred<-predict.C5.0(tree,newdata = testdf,type = "prob")
#ks(pred[,2],testdf[,"def"],plots = TRUE)%>%print()
summary(tree)
#############################################################rules
write.csv(df,"df.csv")
table(gid[,"num_int_city_hen_totaldays"])
