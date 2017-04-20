#' doing single numeric variable analysis
#' 
#' this function will doing single numeric variable analysis
#'
#' @param df the dataframe
#' @param targ the label variable
#' @export
#' @examples
#' single_numvar_analysis(value,target,timestp,varname,tcp=NULL,nums=500,nsp=20,lg=F,ord=F)
single_numvar_analysis<-function(value,target,timestp,varname,tcp=NULL,nums=500,nsp=20,lg=F,ord=F,miss=-9999){
  adf<-data.frame(val=as.numeric(value),targ=target,tistp=as.numeric(timestp))
  temp<-stran_sign_handle(adf,miss)
  adf<-adf[complete.cases(temp),]
  adf<-adf[order(adf[,3]),]
  pre<-round(length(adf[,1])*0.5)
  if(is.null(tcp))
    tcp<-adf[pre,3]
  df<-filter(adf,tistp<=tcp)
  adf<-filter(adf,tistp>tcp)
  sep.point<-get_sep_point(df[,1],nums,nsp)
  odds<-get_odds(sep.point,df[,1:2])
  aodds<-get_odds(sep.point,adf[,1:2])
  if(ord){
    odds[order(odds[,2]),2]<-1:length(odds[,2])
    aodds[order(aodds[,2]),2]<-1:length(odds[,2])
  }
  id<-odds[,1]
  if(lg)
    id<-log(id-min(id)+1)
  pdf<-data.frame(id,odds[,2],aodds[,2],odds[,2]-aodds[,2])
  tcp<-as.Date(tcp,origin = "1970-1-1")
  names(pdf)[2:4]<-c(paste(c("before","after"),tcp),'odds gap')
  pdf<-melt(pdf,id.vars="id")
  print(ggplot(pdf[as.character(pdf[,2])!='odds gap',],aes(id,value,colour=variable))+geom_point()+stat_smooth()+xlab(varname)+ylab('default odds'))
  print(ggplot(pdf[as.character(pdf[,2])=='odds gap',],aes(id,value,colour=variable))+geom_point()+stat_smooth()+xlab(varname)+ylab('odds gap'))
}

#' doing time trends analysis
#' 
#' this function will doing time trends analysis
#'
#' @param df the dataframe
#' @param targ the label variable
#' @export
#' @examples
#' time_trend_analysis(target,timestp,nums=500,nsp=20,events=NULL,timepoint=NULL)
time_trend_analysis<-function(target,timestp,nums=500,nsp=20,events=NULL,timepoint=NULL,miss=-9999){
  df<-data.frame(tistp=as.numeric(timestp),targ=target)
  temp<-stran_sign_handle(df,miss)
  df<-df[complete.cases(temp),]
  df<-df[order(df[,1]),]
  values<-table(df[,1])
  df<-define_types(df,as.character)
  aodds<-prop.table(table(df[,2]))['1']
  sep.point<-get_sep_point(df[,1],nums,nsp)
  odds<-get_odds(sep.point,df[,1:2])
  id<-as.Date(odds[,1],origin = '1970-01-01')
  pdf<-data.frame(id,odds[,2])
  names(pdf)[2]<-'timetrends'
  p<-ggplot(pdf,aes(id,timetrends))+stat_smooth()+xlab('times')+ylab('default odds')+ geom_hline(yintercept =aodds,colour='green',linetype = "dotdash")+ scale_x_date(date_labels = "%Y %b")
  if(!is.null(timepoint)&!is.null(events)){
    mp<-vapply(timepoint,function(s)which.min(abs(pdf[,1]-s)),1)
    data <- data.frame(timepoint,events,odds=pdf[mp,2])
    p<-p+ geom_text(data=data,aes(timepoint,odds,label = events),vjust=0,hjust=0,size=5)+
    geom_point(aes(timepoint,odds),data = data,size = 5, alpha=0.5, colour="red")
  }
  print(p)
}

#' doing category variable analysis
#' 
#' this function will doing category variable analysis
#'
#' @param df the dataframe
#' @param targ the label variable
#' @export
#' @examples
#' single_catvar_analysis(value,target,timestp,varname,ord=T,tcp=NULL)
single_catvar_analysis<-function(value,target,timestp,varname,ord=T,tcp=NULL,miss=-9999){
  adf<-data.frame(val=value,targ=target,tistp=as.numeric(timestp))
  temp<-stran_sign_handle(adf,miss)
  adf<-adf[complete.cases(temp),]
  adf<-adf[order(adf[,3]),]
  pre<-round(length(adf[,1])*0.5)
  features<-as.character(unique(adf[,1]))
  if(is.null(tcp))
    tcp<-adf[pre,3]
  df<-filter(adf,tistp<=tcp)
  adf<-filter(adf,tistp>tcp)
  num<-table(df[,1])[features]
  num<-cut(num,c(Inf,-Inf,quantile(num,(1:5)*0.2)))
  ods<-prop.table(table(df[,1],df[,2]),1)[,'1'][features]
  aods<-prop.table(table(adf[,1],adf[,2]),1)[,'1'][features]
  ods[is.na(ods)]<-0
  aods[is.na(aods)]<-0
  ####
  if(ord){
    ods[order(ods)]<-1:length(ods)
    aods[order(aods)]<-1:length(ods)
  }
  pdf<-data.frame(ods,aods,lab=features,num)
  tcp<-as.Date(tcp,origin = "1970-1-1")
  p<-ggplot(pdf,aes(ods,aods))+geom_point()+stat_smooth()
  p<-p+xlab(paste('before',tcp))+ylab(paste('after',tcp))
  p<-p+geom_abline(slope = 1,colour='red',linetype = "dotdash")
  p<-p+geom_text(data=pdf,aes(ods,aods,label = lab),vjust=0,hjust=0,size=5)+
    geom_point(aes(ods,aods,size = num,colour=num),data = pdf, alpha=0.5)
    print(p)
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
graph_analysis<-function(df,var.list,target,bins=10,df2=NULL){
  df<-df[!is.na(df[,target]),]
  default<-df[,target]%>%as.factor()
  default2<-df2[,target]%>%as.factor()
  for(i in var.list){
    name<-paste(i,'.png',sep='')
    vec<-df[,i]
    cutp<-c(-Inf,Inf,quantile(vec,seq(0,1,1/bins)))%>%unique
    vec<-cut(vec,cutp)
    gdf<-data.frame(vec,default)
    g11<-ggplot(gdf,aes(vec,fill=default))+geom_bar()+xlab(i)+ylab("数量")
    g12<-ggplot(gdf,aes(vec,fill=default))+geom_bar(position ="fill")+xlab(i)+ylab("百分比")
    if(!df2%>%is.null){
      vec<-df2[,i]
      vec<-cut(vec,cutp)
      gdf<-data.frame(vec,default2)
      g21<-ggplot(gdf,aes(vec,fill=default2))+geom_bar()+xlab(i)+ylab("数量")
      g22<-ggplot(gdf,aes(vec,fill=default2))+geom_bar(position ="fill")+xlab(i)+ylab("百分比")
      png(name)
      multiplot(g11,g12,g21,g22,cols=2)
      dev.off()
    }else{
      png(name)
      multiplot(g11,g12,cols=2) 
      dev.off()
    }

  }
}
