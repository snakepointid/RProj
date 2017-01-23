library(rpart)
library(Matrix)
library(xgboost)
library(dplyr)
library(dplyr)

robust_df<-function(df,targ,min){
  temp<-sepe_var(df,targ)
  dis.var<-temp[[1]]
  for(i in dis.var){
    df[,i]<-sep_vec(df[,i],df[,"def"],get_IV,mins = min)
  }
  return(df)
}
get_IV<-function(prob,target){
  prob<-leave_0_point(prob,target)
  mp<-is.na(prob)%>%which()
  prob[mp]<-"missing"
  
  temp<-table(target,prob)
  bad.vec<-temp[2,]
  good.vec<-temp[1,]
  
  bads<-sum(bad.vec)
  goods<-sum(good.vec)
  
  bad.woe<-bad.vec/bads
  good.woe<-good.vec/goods
  woe<-log(bad.woe/good.woe)
  dif<-bad.woe-good.woe
  ivi<-dif*woe
  iv<-sum(ivi)
  return(iv)
}

get_MI<-function(predict,target){
  predict<-leave_0_point(predict,target)
  np<-which(is.na(predict))
  vec[np]<-"missing"
  
  joint<-table(target,predict)%>%prop.table()
  temp<-table(predict)%>%prop.table()
  margin1<-matrix(temp,nrow=1)
  temp<-table(target)%>%prop.table()
  margin2<-matrix(temp,ncol=1)
  margin<-margin2%*%margin1
  temp<-joint/margin
  temp<-log(temp)
  temp<-temp*joint
  MI<-sum(temp)
  return(MI)
}
get_IG<-function(vec,target,miss=TRUE){
  vec<-leave_0_point(vec,target)
  np<-which(is.na(vec))
  vec[np]<-"missing"
  
  tb<-prop.table(table(target))
  ptb<--tb*log2(tb)
  I0<-sum(ptb)
  tb<-prop.table(table(target,vec),2)
  ptb<--tb*log2(tb)
  ptb<-apply(ptb,2,function(s)sum(s,na.rm = T))
  tb<-prop.table(table(vec))
  I1<-sum(ptb*tb)
  IG<-I0-I1
  ptb<--tb*log2(tb)
  IIG<-sum(ptb)
  IGR<-IG/IIG
  return(IGR)
}

get_var_info<-function(df,vars,targ,way,decreasing=T){
  vars<-vars[vars!=targ]
  InFo<-apply(df[,vars],2,function(s)way(s,df[,targ]))
  names(InFo)<-vars
  return(InFo[order(InFo,decreasing = decreasing)])
}

leave_0_point<-function(prob,target){
  prob<-prob%>%as.character()
  
  temp<-table(target,prob)
  te<-names(temp[1,])
  mp<-which(temp[1,]==0|temp[2,]==0)
  last<-length(temp[1,])
  all<-1:last
  nmp<-all[!all%in%mp]
  big<-max(nmp)
  for(i in mp){
    if(i>big){
      cp<-te[i]
      c2p<-te[big]
      prob[prob==cp]<-c2p
    }else{
      cp<-te[i]
      c2p<-te[i+1]
      prob[prob==cp]<-c2p
    }
  }
  return(prob)
}


info_matr<-function(vars,df,targ,way){
  l<-length(vars)
  info.matr<-matrix(NA,nrow=l,ncol=l,dimnames = list(vars,vars))
  c.p.matr<-matrix(NA,nrow=l,ncol=l,dimnames = list(vars,vars))
  for(i in 1:(l-1)){
    print(i)
    temp.i<-way(df[,vars[i]],df[,targ])
    for(j in (i+1):l){
      print(j)
      vec<-paste(df[,vars[i]],df[,vars[j]])
      temp.j<-way(vec,df[,targ])
      temp<-temp.j-temp.i
      c.p<-nor_test(df[,vars[i]],df[,vars[j]])
      c.p.matr[vars[i],vars[j]]<-c.p
      info.matr[vars[i],vars[j]]<-temp
    }
  }
  return(list(info.matr,c.p.matr))
}


keep.or.drop<-function(info.matr,a){
  while(2>1){
    l<-length(info.matr[,1])
    if(length(info.matr[2,])<a)break
    drop.p<-apply(info.matr[,2:l],2,function(s)min(s,na.rm=T))%>%which.min()+1
    info.matr<-info.matr[,-drop.p]
    info.matr<-info.matr[-drop.p,]
    paste("还剩",l)%>%print()
  }
  keep.vars<-colnames(info.matr)
  return(keep.vars)
}

############################################################连续变量相关性检验
num_test<-function(vec,tar){
  ft<-summary(aov(vec~tar))[[1]][1,5]
  return(ft)
}
############################################################离散变量相关性检验
nor_test<-function(vec,tar){
  pv<-chisq.test(vec,tar)$p.value
  return(pv)
}
############################################################共线性检验
cor_test<-function(df,cor.rate,info.vec,method="pearson"){
  useless<-c()
  var.vec<-names(info.vec)
  cor.matrix<-cor(df[,var.vec],method=method)%>%abs()
  drop.p<-apply(cor.matrix,2,function(s)which(s>cor.rate))
  for(i in 1:length(var.vec)){
    temp<-drop.p[[i]]
    useless<-c(useless,temp[temp>i])
  }
  return(unique(var.vec[useless]))
}
#一般ks求法
ks<-function(prob,target,plots=FALSE){
  library(ggplot2)
  library(reshape2)
  temp<-table(target,prob)
  bad.vec<-temp[2,]
  good.vec<-temp[1,]
  
  bads<-sum(bad.vec)
  goods<-sum(good.vec)
  
  bad.cum<-cumsum(bad.vec)
  good.cum<-cumsum(good.vec)
  
  bad.ks<-bad.cum/bads
  good.ks<-good.cum/goods
  
  ks<-good.ks-bad.ks
  if(plots){
    id<-names(bad.ks)
    id<-as.numeric(id)
    df<-data.frame(id,bad.ks,good.ks,ks)
    names(df)[2:3]<-c("坏客户累积占比","好客户累积占比")
    df<-melt(df,id.vars="id")
    print(ggplot(df,aes(id,value,colour=variable))+geom_line()+xlab("")+ylab("百分比"))
  }
  wh<-which.max(abs(ks))
  big_ks<-ks[wh]
  #print(paste("the biggest ks is:",big_ks))
  #print(paste("the seperation point is:",id[wh]))
  return(big_ks)
}
library(dplyr)

sep_vec<-function(prob,target,info_way=get_IV,mins=500){
  type<-class(prob)
  if(type!="numeric"){
    temp<-dis2con(prob,target)
    prob<-temp[[1]]
    proj.info<-temp[[2]]
  }
  op<-table(prob)%>%names()%>%as.numeric()
  max.iv<-0
  old.points<-c()
  sep.p<-c()
  while(2>1){
    sep<-find_best_point(prob,op,old.points,target,info_way,mins,max.iv=max.iv)
    old.points<-sep[[2]]
    new.iv<-sep[[1]]
    if(max.iv<new.iv){
      max.iv<-new.iv
      sep.p<-old.points
    }else{
      print(paste("目前分成",length(sep.p)+1,"箱"))
      break
    }
  }
  if(length(sep.p)==0){
    prob<-prob%>%as.character()
    mp<-which(is.na(prob))
    prob[mp]<-"missing"
    prob[-mp]<-"valued"
    return(prob%>%as.factor())
    break
  }
  print(max.iv)
  if(type=="factor"){
    prob<-dis2dis(prob,proj.info,sep.p)
  }else{
    sep.p<-c(sep.p,-Inf,Inf)
    prob<-cut(prob,sep.p)
  }
  return(prob)
}
find_best_point<-function(prob,op,old.points,target,info_way,mins,max.iv){
  op<-op[!op%in%old.points]
  best.p<-c()
  for(i in op){
    new.points<-c(i,old.points,-Inf,Inf)
    temp<-cut(prob,new.points)
    good.count<-table(temp,target)[,1]%>%min()
    bad.count<-table(temp,target)[,2]%>%min()
    all.count<-table(temp)%>%min()
    if(all.count<mins|bad.count<1|good.count<1){
      new.iv<-0}else{
        new.iv<-info_way(temp,target)}
    if(max.iv<new.iv){
      max.iv<-new.iv
      best.p<-i
    }
  }
  old.points<-c(best.p,old.points)
  return(list(max.iv,old.points))
}

dis2con<-function(prob,target){
  prob<-prob%>%as.character()
  temp<-table(prob,target)
  temp<-temp[order(temp[,2]/temp[,1]),]
  disn<-names(temp[,1])
  proj<-prob
  j<-1
  for(i in disn){
    proj[which(prob==i)]<-j
    j<-j+1
  }
  proj.vec<-1:(j-1)
  names(proj.vec)<-disn
  return(list(proj%>%as.numeric(),proj.vec))
}

dis2dis<-function(prob,pi,sep){
  proj<-prob%>%as.character()
  sep<-sep[order(sep)]
  sep<-c(sep,Inf)
  for(i in sep){
    tname<-names(pi[which(pi<=i)])
    name<-c()
    for(n in tname){
      name<-paste(name,n,sep="_")
    }
    proj[which(prob<=i)]<-name
    prob[which(prob<=i)]<-NA
    pi[which(pi<=i)]<-NA
  }
  return(proj%>%as.factor())
}





missing_count<-function(df){
  l<-length(df[,1])
  miss<-apply(df,2,function(s)which(is.na(s))%>%length()/l)
  return(miss[order(miss)])
}

single.value_count<-function(df,targ){
  l<-length(df[,1])
  vars<-names(df)
  vars<-vars[vars!=targ]
  miss<-apply(df[,vars],2,function(s)table(s)%>%max()/l)
  return(miss[order(miss)])
}

miss_handle<-function(df,num.handle=F){
  for(i in names(df)){
    if(is.factor(df[,i])){
      df[,i]<-as.character(df[,i])
      mp<-is.na(df[,i])%>%which()
      df[mp,i]<-"missing"
      df[,i]<-as.factor(df[,i])
    }else{
      if(num.handle){
        temp<-df[,i]
        mp<-is.na(df[,i])%>%which()
        df[mp,i]<--1}
    }
  }
  return(df)
}


trans2miss<-function(df,value){
  for(i in names(df)){
    tp<-which(df[,i]==value)
    df[tp,i]<-NA
  }
  return(df)
}


#################根据相关文档获取变量类型
define_type<-function(df,var.type){
  var.t<-c()
  for(i in names(df)){
    type<-var.type[var.type[,1]==i,2]
    if(length(type)>0){
      if(type%in%c("分类变量", "类别变量","字符变量")){
        df[,i]<-as.character(df[,i])%>%as.factor()
      }else{
        df[,i]<-as.character(df[,i])%>%as.numeric()
        #paste(i,"匹配为：",type,"【值为：",df[10,i])%>%print()
      }
    }else{
      paste(i,"并没有得到匹配")%>%print()
      var.t<-c(var.t,i)
    }
  }
  return(list(df,var.t))
}
########################根据取值范围定义
naive_defi_type<-function(df,dis.list,a){
  for(i in names(df)){
    df[,i]<-as.character(df[,i])%>%as.factor()
    l<-levels(df[,i])%>%length()
    tryCatch({
      if(l>a)df[,i]<-as.character(df[,i])%>%as.numeric()},warning = function(w) {
        print(paste(i,"字符型不能转成数值型"))
      }
    )
  }
  for(i in dis.list){
    df[,i]<-as.character(df[,i])%>%as.factor()
  }
  return(df)
}
###########################计算不同取值
count_distinct_value<-function(df,var.list){
  n<-1
  for(i in var.list){
    vec<-df[,i]
    temp<-table(vec)%>%names()
    l<-length(temp)
    va.vec<-c(i,l,temp[1],temp[l])
    if(n==1)va.df<-va.vec else
      va.df<-rbind(va.df,va.vec)
    n<-n+1
  }
  va.df<-data.frame(va.df,row.names = NULL)
  va.df[,2]<-va.df[,2]%>%as.character()%>%as.numeric()
  names(va.df)<-c("变量","数目","第一取值","最后取值")
  va.df<-va.df[order(va.df[,2]),]
  return(va.df)
}
###########################分布获取名义变量和数值变量
sepe_var<-function(df,target){
  dis<-c()
  cont<-c()
  for(i in names(df)){
    if(i!=target){
      if(class(df[,i])=="factor")
        dis<-c(dis,i)else cont<-c(cont,i)
    }
  }
  return(list(dis,cont))
}


cv_balance<-function(df,folds,targ){
  g_b<-as.character(df[,targ])
  cv.list<-list()
  remain.g<-which(g_b=="0")
  remain.b<-which(g_b=="1")
  
  g.ns<-length(remain.g)
  g.ns<-g.ns/folds
  b.ns<-length(remain.b)
  b.ns<-b.ns/folds
  for(i in 1:folds){
    samp.g<-sample(remain.g,g.ns,replace = F)
    samp.b<-sample(remain.b,b.ns,replace = F)
    all<-c(samp.b,samp.g)
    cv.list[[i]]<-sample(all,length(all),replace = F)
    remain.g<-remain.g[!remain.g%in%samp.g]
    remain.b<-remain.b[!remain.b%in%samp.b]
  }
  return(cv.list)
}


iv_sig_filter<-function(df,method,cor.rate=0.7,iv.level=0.02,sig.level=0.05){
  set.seed(123)
  temp<-sepe_var(df,"def")
  dis.var<-temp[[1]]
  con.var<-temp[[2]]
  iv.dis<-get_var_info(df,dis.var,"def",get_IV)
  sig.con<-get_var_info(df,con.var,"def",num_test,decreasing=F)
  use.dis<-names(iv.dis[iv.dis>iv.level])
  sig.con<-sig.con[sig.con<sig.level]
  use.con<-names(sig.con)
  useless<-cor_test(df,cor.rate,sig.con,method)
  useful<-c(use.dis,use.con)
  useful<-useful[!useful%in%useless]
  return(useful)}

iv_ks_filter<-function(df,method,cor.rate=0.7,iv.level=0.02,ks.level=0.06){
  set.seed(123)
  temp<-sepe_var(df,"def")
  dis.var<-temp[[1]]
  con.var<-temp[[2]]
  iv.dis<-get_var_info(df,dis.var,"def",get_IV)
  ks.con<-get_var_info(df,con.var,"def",ks)
  use.dis<-names(iv.dis[iv.dis>iv.level])
  ks.con<-ks.con[ks.con<ks.level]
  use.con<-names(ks.con)
  useless<-cor_test(df,cor.rate,ks.con,method)
  useful<-c(use.dis,use.con)
  useful<-useful[!useful%in%useless]
  return(useful)}

variable_select_xgboost<-function(df,useful,para,nround,a,loop){
  count<-1
  l.index<-1
  remain<-useful
  keep<-c()
  test.ks.serial<-c()
  train.ks.serial<-c()
  tree.num.serial<-c()
  vars.keep.serial<-list()
  old<-xg_cv(df,useful,para,nround)[1]
  while(2>1){
    tryCatch({
      if(length(remain)==0){
        if(count>=loop)break
        set.seed(123)
        remain<-sample(keep,length(keep),replace = F)
        keep<-c()
        old<-xg_cv(df,remain,para,nround)[1]
        count<-count+1
      }
      temp<-xg_cv(df,c(remain[-1],keep),para,nround)
      new<-temp[1]
      print(paste("不考虑",remain[1],"的结果"))
      print(new)
      if(old-new>a){
        keep<-c(keep,remain[1])
      }else{
        print(paste("因为",remain[1],"没有贡献，所以去掉"))
        old<-new
        test.ks.serial<-c(test.ks.serial,temp[1])
        train.ks.serial<-c(train.ks.serial,temp[2])
        tree.num.serial<-c(tree.num.serial,temp[3])
        vars.keep.serial[[l.index]]<-c(remain[-1],keep)
        l.index<-l.index+1
      }
      remain<-remain[-1]
    },error=function(e){
      use<-c(use,i+1)
      print("貌似出了点问题")
    }
    )
  }
  id<-lapply(vars.keep.serial,length)%>%unlist()
  df<-data.frame(id,test.ks.serial,train.ks.serial,tree.num.serial)
  names(df)[2:3]<-c("测试ks","训练ks")
  ks.df<-melt(df[,-4],id.vars="id")
  print(ggplot(ks.df,aes(id,value,colour=variable))+geom_line()+xlab("变量数目")+ylab("ks"))
  print(ggplot(df,aes(id,tree.num.serial))+geom_line()+xlab("变量数目")+ylab("树的数目"))
  return(vars.keep.serial)
}

library(xgboost)
library(dplyr)
library(Matrix)
xg_cv<-function(df,useful,para,nround){
  train.mat <- sparse.model.matrix(def~., data = df[,c(useful,"def")])
  train.l<-df[,"def"]%>%as.character()%>%as.numeric()
  
  dtrain <- xgb.DMatrix(data =train.mat,label = train.l)
  set.seed(123)
  bst <- xgb.cv(params=para, data=dtrain,nround =nround,verbose = F,nfold=4,feval=evalerror)
  wp<-bst$test.ks.mean%>%which.max()
  
  test.ks<-bst$test.ks.mean[wp]
  train.ks<-bst$train.ks.mean[wp]
  return(c(test.ks,train.ks,wp))
}

evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- ks(preds,labels)
  return(list(metric = "ks", value = err))
}

para <- list(max.depth =3, eta = 0.07,objective="binary:logistic")

controls.rpart<-rpart.control(minsplit =5,
                              cp =0.001,
                              maxcompete =1,
                              maxsurrogate =1,
                              usesurrogate=1,
                              xval =4,
                              surrogatestyle =1,
                              maxdepth = 8)


