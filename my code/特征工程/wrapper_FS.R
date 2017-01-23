SBS_xgboost<-function(df,useful,para,a){
  temp<-get_df_info(df,useful,"def",500,way="iv")
  temp<-temp[order(temp,decreasing = F)]
  useful<-names(temp)
  nround<-300
  temp<-xg_cv(df,useful,para,nround)
  index<-1
  test.ks.serial<-temp[1]
  train.ks.serial<-temp[2]
  trees<-temp[3]
  vars.keep.serial<-list()
  vars.keep.serial[[index]]<-useful
  old<-temp[1]
  for(i in useful){
    tryCatch({
      useful<-useful[useful!=i]
      temp<-xg_cv(df,useful,para,nround)
      new<-temp[1]
      trees<-c(trees,temp[3])
      nround<-mean(trees)+50
      print(paste("不考虑",i,"的结果"))
      print(new)
      if(old-new>a){
        useful<-c(useful,i)
      }else{
        print(paste("因为",i,"没有贡献，所以去掉"))
        old<-new
        test.ks.serial<-c(test.ks.serial,temp[1])
        train.ks.serial<-c(train.ks.serial,temp[2])
        index<-index+1
        vars.keep.serial[[index]]<-useful
        sbs_curve(test.ks.serial,train.ks.serial,vars.keep.serial)
      }
    },error=function(e){
      useful<-c(useful,i)
      print("出了点问题")
    }
    )
  }
  return(list(test.ks.serial,train.ks.serial,vars.keep.serial))
}

sbs_curve<-function(test.s,train.s,vars.s){
  id<-lapply(vars.s,length)%>%unlist()
  df<-data.frame(id,test.s,train.s)
  names(df)[2:3]<-c("测试ks","训练ks")
  ks.df<-melt(df,id.vars="id")
  print(ggplot(ks.df,aes(id,value,colour=variable))+geom_line()+xlab("变量数目")+ylab("ks"))
}






