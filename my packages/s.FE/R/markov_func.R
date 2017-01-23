#' fit the Markov
#'
#' fit the Markov
#' 
#' @param markov.mat the Markov lineage
#' @param targ.vec the target vect
#' @param lowest  lowest nums
#' @param ddr=F ddr

#' @export
#' @examples
#' 
#' markov_fit(markov.mat,targ.vec,30,ddr=T)
markov_fit<-function(markov.mat,targ.vec,lowest,ddr=T){
  ###alarm about input types
  if(!is.character(targ.vec))
    stop('target vector must be character')
  targ.vec<-targ.vec[!is.na(targ.vec)]
  mp<-which(markov.mat[,1]%in%names(targ.vec))
  markov.mat<-markov.mat[mp,]
  targ.vec<-targ.vec[markov.mat[,1]]
  markov.mat<-cbind(markov.mat,targ.vec)
  ######whether ddr
  if(ddr){
    mp<-which(markov.mat[,2]==markov.mat[,3])
    markov.mat<-markov.mat[-mp,]
  }
    
  #find unstable pair
  tmp1<-table(markov.mat[,2])
  tmp2<-table(markov.mat[,2],markov.mat[,3])
  lws<-min(sort(unique(tmp1))[lowest],sort(unique(tmp2))[lowest],na.rm=T)
  unstal.init<-tmp1<lws
  unstal.mat<-tmp2<lws
  #####normal
  pm<-which(markov.mat[,4]=='1')
  nm<-which(markov.mat[,4]=='0')
  posi.mat<-markov.mat[pm,2:3]
  negt.mat<-markov.mat[nm,2:3]
  ###
  pmp<-which(posi.mat[,2]=='end')
  nmp<-which(negt.mat[,2]=='end')
  pmp<-c(1,pmp[-length(pmp)]+1)
  nmp<-c(1,nmp[-length(nmp)]+1)
  posi.init<-table(posi.mat[pmp,1])%>%prop.table()
  negt.init<-table(negt.mat[nmp,1])%>%prop.table()

  posi.mat<-table(posi.mat[,1],posi.mat[,2])%>%prop.table(.,1)
  negt.mat<-table(negt.mat[,1],negt.mat[,2])%>%prop.table(.,1)
  ###################reshape those wide df to long df
  posi.mat <- melt(posi.mat,id=rownames(posi.mat),na.rm=TRUE)
  negt.mat <- melt(negt.mat,id=rownames(negt.mat),na.rm=TRUE)
  unstal.mat <- melt(unstal.mat,id=rownames(unstal.mat),na.rm=TRUE)
  ###################
  posi.vec<-posi.mat[,3]
  names(posi.vec)=paste(posi.mat[,1],posi.mat[,2],sep='#')
  negt.vec<-negt.mat[,3]
  names(negt.vec)=paste(negt.mat[,1],negt.mat[,2],sep='#')
  unstal.vec<-unstal.mat[,3]
  names(unstal.vec)=paste(unstal.mat[,1],unstal.mat[,2],sep='#')
  ######################################
  key<-names(unstal.vec)
  un.key<-names(unstal.vec)[unstal.vec]
  nekey<-key[!key%in%names(posi.vec)]
  add.v<-rep(0,length(nekey))
  names(add.v)<-nekey
  posi.vec<-c(posi.vec,add.v)
  posi.vec[un.key]<-1
  nekey<-key[!key%in%names(negt.vec)]
  add.v<-rep(0,length(nekey))
  names(add.v)<-nekey
  negt.vec<-c(negt.vec,add.v)
  negt.vec[un.key]<-1
  key<-names(unstal.init)
  un.key<-names(unstal.init)[unstal.init]
  posi.init[key[!key%in%names(posi.init)]]<-0
  posi.init[un.key]<-1
  negt.init[key[!key%in%names(negt.init)]]<-0
  negt.init[un.key]<-1
  rc<-list('posi.init'=posi.init,'negt.init'=negt.init,
           'posi.vec'=posi.vec,'negt.vec'=negt.vec)
  return(rc)
}

#' transform the markov fit
#'
#' transform the markov fit
#' 
#' @param markov.mat the Markov lineage
#' @param markov.fit the fitted markov
#' @param targ.vec the target vect
#' @param init  init
#' @export
#' @examples
#' 
#' markov_transform(markov.mat,markov.fit,targ.vec,init=F)

markov_transform<-function(markov.mat,markov.fit,targ.vec){
  markov.mat<-data.frame(markov.mat,stringsAsFactors = F)
  mp<-which(markov.mat[,1]%in%names(targ.vec))
  markov.mat<-markov.mat[mp,]
  ###############get table names
  key<-paste(markov.mat[,2],markov.mat[,3],sep='#')
  temp<-data.frame(Idx=markov.mat[,'Idx'],posi=markov.fit$posi.vec[key],negt=markov.fit$negt.vec[key])
  temp<-miss_handle(temp,1)
  gb <- group_by(temp,Idx)
  prod.p <- summarise(gb,posi.p=prod(posi),negt.p=prod(negt))
  igb<-group_by(markov.mat,Idx)
  init.k <- summarise(igb,key=first(value))
  key<-init.k$key%>%as.character()
  init.p<-data.frame(Idx=init.k$Idx,init.posi=as.numeric(markov.fit$posi.init[key]),
                     init.negt=as.numeric(markov.fit$negt.init[key]))
  init.p<-miss_handle(init.p,1)
  ############################################
  posi.init<-prod.p$posi.p*init.p$init.posi
  negt.init<-prod.p$negt.p*init.p$init.negt

  negt.init[negt.init==0]<-min(negt.init[negt.init!=0])*0.1
  pp<-posi.init/negt.init

  names(pp)<-prod.p$Idx%>%as.character()
  return(pp)
}
#' transform the markov fit
#'
#' transform the markov fit
#' 
#' @param gb the Markov lineage matrix
#' @param ddr whether delete duplicate rows
#' @param train.y the train target
#' @param test.y the test target
#' @param lowest.vec best lowest
#' @export
#' @examples
#' 
#' get_the_mk_vec(gb,ddr=F,targ.vec,target.y,1:300)


get_the_mk_vec<-function(markov_mat,ddr,train.y,test.y,lowest.vec,diff.low){
  old<-0
  best.vec<-0
  for(i in lowest.vec){
    m.f<-markov_fit(markov_mat,train.y,i,ddr)
    out<-markov_transform(markov_mat,m.f,c(train.y,test.y))
    train.ks<-ks(out,train.y[names(out)])
    test.ks<-ks(out,test.y[names(out)])
    new<-(train.ks+test.ks)/2
    diff<-abs(train.ks-test.ks)
    if(new>old&diff<diff.low){
      old<-new
      print(paste('lowest is',i,'ks is ',new))
      best.vec<-out
    }
  }
  return(best.vec)
  }