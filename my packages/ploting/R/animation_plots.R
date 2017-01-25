#' get a animate numeric plots
#' 
#' this function will get a animate numeric plots
#'
#' @param value the dataframe
#' @param target the label variable
#' @param timestp the label variable
#' @param varname the label variable
#' @param dir the label variable
#' @export
#' @examples
#' numvar_animation(value,target,timestp,varname,dir,nums=500,nsp=20,tnums=5000,npics=300,lg=F)
numvar_animation<-function(value,target,timestp,varname,dir='C:/Users/uij/Documents/R/image/',nums=500,nsp=20,tnums=5000,npics=300,lg=F,miss=-9999){
  adf<-data.frame(val=as.numeric(value),targ=target,tistp=as.numeric(timestp))
  temp<-stran_sign_handle(adf,as.character(miss))
  adf<-adf[complete.cases(temp),]
  adf<-adf[order(adf[,3]),]
  ##########################
  time.sep<-get_sep_point(adf[,3],tnums,npics)
  ##########################
  for(i in names(time.sep)){
    sub.time.df<-filter(adf,tistp%in%time.sep[[i]])
    if(i==names(time.sep)[1]){
      value.sep<-get_sep_point(sub.time.df[,1],nums,nsp)
      odds<-get_odds(value.sep,sub.time.df[,1:2])
      aodds<-get_odds(value.sep,adf[,1:2])
      ymax<-max(aodds[,2])*1.3
      ymin<-min(aodds[,2])*0.7
    }
    odds<-get_odds(value.sep,sub.time.df[,1:2])
    if(lg){
      odds[,1]<-log(odds[,1]+1)
      aodds[,1]<-log(aodds[,1]+1)
      }
    time<-as.Date(as.numeric(i),origin="1970-01-01")
    mth<-months(time)
    tdf<-data.frame(value=(max(aodds[,1])+min(aodds[,1]))/2,odds=ymax*0.95,mth)
    name<-paste(dir,varname,time,'.png',sep='')
    png(name)
    p<-ggplot(odds,aes(value,odds))+stat_smooth()
    p<-p+stat_smooth(data=aodds,aes(value,odds),colour='red')
    p<-p+xlab(paste(varname,'in',time))+ylab('default odds')+ylim(ymin,ymax)+theme(legend.position='none')
    p<-p+geom_text(data=tdf,aes(value,odds,label =mth),vjust=0,hjust=0,size=10)
    print(p)
    dev.off()
  }
}

#' get a animate catgory variable plots
#' 
#' this function will get a animate catgory variable plots
#'
#' @param value the dataframe
#' @param target the label variable
#' @param timestp the label variable
#' @param varname the label variable
#' @param dir the label variable
#' @export
#' @examples
#' catvar_animation(value,target,timestp,varname,dir,tnums=5000,npics=300)
catvar_animation<-function(value,target,timestp,varname,dir='C:/Users/uij/Documents/R/image/',tnums=5000,npics=300,miss=-9999){
  adf<-data.frame(val=value,targ=target,tistp=as.numeric(timestp))
  temp<-stran_sign_handle(adf,as.character(miss))
  adf<-adf[complete.cases(temp),]
  adf<-adf[order(adf[,3]),]
  features<-as.character(unique(adf[,1]))
  ##########################
  aodds<-prop.table(table(adf[,1],adf[,2]),1)[,'1'][features]
  aodds[is.na(aodds)]<-0
  xmax<-max(aodds)*1.5
  time.sep<-get_sep_point(adf[,3],tnums,npics)
  ##########################
  for(i in names(time.sep)){
    sub.time.df<-filter(adf,tistp%in%time.sep[[i]])
    odds<-prop.table(table(sub.time.df[,1],sub.time.df[,2]),1)[,'1'][features]
    odds[is.na(odds)]<-0
    num<-table(sub.time.df[,1])[features]
    num.dis<-cut(num,c(Inf,-Inf,quantile(num,(1:5)*0.2)))
    pdf<-data.frame(aodds,odds,num)
    time<-as.Date(as.numeric(i),origin="1970-01-01")
    name<-paste(dir,varname,time,'.png',sep='')
    png(name)
    p<-ggplot(pdf,aes(odds,aodds,size = num,colour=num.dis))+geom_point(alpha=0.5)
    p<-p+xlab(paste(varname,'in',time))+ylab('overall odds')+xlim(0,xmax)+theme(legend.position='none')
    p<-p+scale_size_area(max_size = 30)+ scale_colour_brewer(palette = "Set1")
    print(p)
    dev.off()
  }
}



#' get sep point
#' 
#' this function will get sep point
#'
#' @param vec the numeric vector
#' @param minnums the minimul observe numbers
#' @param sepnums the sep points
#' @export
#' @examples
#' single_value_count(p2p,"def")
get_sep_point<-function(vec,minnums,sepnums){
  vec<-as.numeric(vec)
  vec_num<-table(vec)
  vec_value<-as.numeric(names(vec_num))
  temp<-vec
  temp[vec>quantile(vec,0.95)]<-quantile(vec,0.95)
  temp[vec<quantile(vec,0.05)]<-quantile(vec,0.05)
  vecp<-unique(quantile(temp,(1:sepnums)/sepnums))
  tmp<-which(vec_value%in%vecp)
  vl<-length(vec_value)
  va.list<-list()
  for(tpi in tmp){
    subnum<-vec_num[tpi]
    fw<-1
    bw<-1
    idx<-0
    while(subnum<minnums){
      idx<-idx+1
      if(tpi+fw<=vl&idx%%2==1){
        subnum<-subnum+vec_num[tpi+fw]
        fw<-fw+1}
      if(tpi-bw>=1&idx%%2==0){
        subnum<-subnum+vec_num[tpi-bw]
        bw<-bw+1
      }
      if(tpi+fw>vl&tpi-bw<1){
        break
      }
    }
    subv<-vec_value[(tpi-bw+1):(tpi+fw-1)]
    va.list[[as.character(vec_value[tpi])]]<-subv
  }
  return(va.list)
}

#' get the df odds
#' 
#' this function will get the df odds
#'
#' @param va.list the sep points list
#' @param df the dataframe need to transfer
#' @export
#' @examples
#' get_odds(va.list,df)
get_odds<-function(va.list,df){
  odds<-c()
  names(df)<-c('value','target')
  for(i in names(va.list)){
    sub.df<-filter(df,value%in%va.list[[i]])
    oddsi<-prop.table(table(sub.df[,2]))['1']
    odds<-c(odds,oddsi)
  }
  odds[is.na(odds)]<-0
  return(data.frame(value=as.numeric(names(va.list)),odds))
}


#' get a animate catgory variable plots
#' 
#' this function will get a animate catgory variable plots
#'
#' @param value the dataframe
#' @param target the label variable
#' @param timestp the label variable
#' @param varname the label variable
#' @param dir the label variable
#' @export
#' @examples
#' catvar_animation(value,target,timestp,varname,dir,tnums=5000,npics=300)

varIV_animation<-function(adf,varn.change,var.clu,dir,lowest=500,tnums=10000,npics=300,miss=-9999){
  names(adf)<-varn.change
  adf[,'targ']<-target
  adf[,'tistp']<-as.numeric(timestp)
  
  temp<-stran_sign_handle(adf,miss)
  adf<-adf[complete.cases(temp[,c('targ','tistp')]),]
  adf<-adf[order(adf[,'tistp']),]
  overall.iv<-c()
  for(i in varn.change){
    print(i)
    if(i%in%c('targ','tistp'))
      next
    temp<-get_IV(adf[,i],adf[,'targ'],lowest)
    overall.iv<-c(overall.iv,temp['iv'])
  }
  xmax<-max(overall.iv)*1.5
  ##########################
  time.sep<-get_sep_point(adf[,'tistp'],tnums,npics)
  ##########################
  for(t in names(time.sep)){
    sub.time.df<-filter(adf,tistp%in%time.sep[[t]])
    sub.overall.iv<-c()
    sub.overall.sn<-c()
    for(i in varn.change){
      print(i)
      if(i%in%c('targ','tistp'))
        next
      temp<-get_IV(sub.time.df[,i],sub.time.df[,'targ'],lowest)
      sub.overall.iv<-c(sub.overall.iv,temp['iv'])
      sub.overall.sn<-c(sub.overall.sn,temp['sn'])
    }
    pdf<-data.frame(imp=length(var.clu):1,sub.overall.iv,sub.overall.sn,var.clu)
    time<-as.Date(as.numeric(t),origin="1970-01-01")
    name<-paste(dir,varname,time,'.png',sep='')
    png(name)
    p<-ggplot(pdf,aes(sub.overall.iv,imp,size =sub.overall.sn,colour=var.clu,labels=vars))+geom_point(alpha=0.5)
    p<-p+xlab(paste("variables' iv in",time))+ylab("variables' importance in model")+xlim(0,xmax)+theme(legend.position='None')
    p<-p+scale_size_area(max_size = 30)+ scale_colour_brewer(palette = "Set3")
    p<-p+geom_text(data=pdf,aes(sub.overall.iv,imp,label =varn.change),colour='black',size=3)
    print(p)
    dev.off()
  }
}