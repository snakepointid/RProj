#' computing the ks and plot 
#' 
#' computing the ks value of a predicted model and can plot graph
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' ks(prob,target)
#' ks(prob,target,plot = T)
ks<-function(prob,target,plots=FALSE,rat=NULL){
  prob<-as.character(prob)%>%as.numeric()
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
    names(df)[2:3]<-c("odds of bad","odds of good")
    df<-melt(df,id.vars="id")
    print(ggplot(df,aes(id,value,colour=variable))+geom_line(size=1.2)+xlab(""))
  }
  if(is.null(rat)){
    wh<-which.max(abs(ks))
    big_ks<-ks[wh]
    }else{
      lks<-length(ks)
      nums<-round(lks*rat)
      mapks<-c()
      
    for(j in 1:lks){
      if(j+nums<lks)
        mapks<-c(mapks,mean(ks[j:(j+nums)]))else
          mapks<-c(mapks,mean(ks[j:lks]))
    }
      wh<-which.max(abs(mapks))
      big_ks<-mapks[wh]
    }
  return(big_ks)
}

#' computing the auc
#' 
#' computing the auc
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' get_auc(prob,target)
get_auc<-function(prob,target){
  target<-as.factor(as.character(target))
  aucs<-auc(roc(prob,target))
  return(aucs)
}


#' get TP,FP,TN,FN
#' 
#' get TP,FP,TN,FN
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' S_Asse(prob,target)
S_Asse<-function(prob,target){
  prob<-as.character(prob)%>%as.numeric()
  target<-as.character(target)%>%as.numeric()
  od<-order(prob)
  prob<-prob[od]
  target<-target[od]
  
  AP<-sum(target)
  AN<-length(prob)-AP
  
  FN<-cumsum(target)
  TP<-AP-FN
  TN<-cumsum(1-target)
  FP<-AN-TN
  df<-data.frame(TP=TP,FP=FP,FN=FN,TN=TN,Pb=prob)
  df$FP<-df$FP+1
  df$TN<-df$TN-1
  return(df)
}
#' plot the roc curve
#' 
#' plot the roc curve
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' rocplot(prob,target)


rocplot<-function(prob,target,smooth=T){
  df<-S_Asse(prob,target)
  TP<-df$TP
  FP<-df$FP
  TN<-df$TN
  FN<-df$FN
  FPR<-FP/(FP+TN)
  PPR<-TP/(TP+FN)
  df<-data.frame(FPR,PPR)
  if(smooth){
    gb<-group_by(df,FPR) 
    gb<-summarise(gb,PPR=mean(PPR))
    df<-data.frame(gb)
    df<-rbind(df,c(0,0),c(1,1))
  }
  df$base<-df$FPR
  df<-melt(df,id.vars=FPR)
  p<-ggplot(df,aes(FPR,value,colour=variable,linetype=variable))+geom_line(size=1.2)
  p<-p+xlab("False positive rate")+ylab("True positive rate")
  print(p)
}

#' plot the f1 score curve
#' 
#' plot the f1 score curve
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' f1plot(prob,target)

f1plot<-function(prob,target,smooth=T){
  df<-S_Asse(prob,target)
  TP<-df$TP
  FP<-df$FP
  TN<-df$TN
  FN<-df$FN
  PRO<-df$Pb%>%as.numeric()
  PRE<-TP/(FP+TP)
  REC<-TP/(TP+FN)
  f1<-2*PRE*REC/(PRE+REC)
  df<-data.frame(PRO,f1)
  df<-df[!is.na(df$f1),]
  if(smooth){
    gb<-group_by(df,PRO) 
    gb<-summarise(gb,f1=mean(f1))
    df<-data.frame(gb)
  }
  cut<-df$PRO[which.max(df$f1)] 
  ddf<-data.frame(prob,target)
  ddf$pred<-'预测正例'
  ddf$pred[ddf$prob<cut]<-'预测负例'
  ddf$ture<-'实际正例'
  ddf$ture[ddf$target==0]<-'实际负例'
  table(ddf$pred,ddf$ture)%>%print()
  p<-ggplot(df,aes(PRO,f1))+geom_line(size=1.2,color='blue')
  p<-p+xlab("probility cut off")+ylab("F1 SCORE")
  print(p)
}



#' plot the f1 score curve
#' 
#' plot the f1 score curve
#'
#' @param prob predicted target probilities
#' @param target the true class of the test data
#' @export
#' @examples
#' f1plot(prob,target)

 
liftChart<-function(prob,target,nums=10){
  df<-data.frame(prob,target)
  df<-df[order(df$prob,decreasing = T),]
  df$tmp<-1
  positive<-cumsum(df$target)
  total<-cumsum(df$tmp)
  model<-positive/total
  base<-sum(df$target)/length(df$target)
  lift<-model/base
  x = seq(0,1,1/nums)
  mp<-quantile(1:length(lift),x)%>%round()
  lift<-lift[mp]
  x<-x*100
  df<-data.frame(x,lift,base=1)
  df<-melt(df,id.vars='x')
  p<-ggplot(df,aes(x,value,colour=variable,linetype=variable))+geom_line(size=1)+geom_point(size=2)
  p<-p+xlab("% of customers contact")+ylab("LIFT")
  print(p)
}

