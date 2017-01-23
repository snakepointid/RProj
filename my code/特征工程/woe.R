#' get a woe transformation fit
#'
#' This function will get a woe transformation fit model
#'
#' @param tdf the discreting dataframe by best seperation
#' @param targ the label variable
#' @export
#' @examples
#' woe_fit(p2p,"def")
woe_fit<-function(tdf,vars,targ){
  woe.fit<-list()
  glo.odds<-table(tdf[,targ])[2]/table(tdf[,targ])[1]
  for(i in vars){
    if(i!=targ){
      temp<-table(tdf[,i],tdf[,targ])
      loc.odds<-temp[,2]/temp[,1]
      odds<-log(loc.odds)-log(glo.odds)
      temp<-0
      names(temp)<-"no.ex"
      odds<-c(odds,temp)
      woe.fit[[i]]<-odds
    }
  }
  return(woe.fit)
}
#' doing a woe transformation 
#'
#' This function will doing a woe transformation 
#'
#' @param tdf the discreting dataframe by best seperation
#' @param woe.fit fitted woe model
#' @export
#' @examples
#' woe_transform(p2p,woe.fit)
woe_transform<-function(tdf,woe.fit){
  woe.v<-names(woe.fit)
  for(i in names(tdf)){
    if(i %in% woe.v){
      vec<-as.character(tdf[,i])
      woe<-woe.fit[[i]]
      woe.n<-names(woe)
      for(j in woe.n){
        p = which(vec==j)
        vec[p]<-woe[j]
      }
      tdf[,i]<-as.numeric(vec)
    }
  }
  return(tdf)
}


#' #' get a bayes transformation fit
#'
#' This function will get a  bayes transformation fit model
#'
#' @param train.df the dataframe with target variable
#' @param whole.df the dataframe without target variable
#' @param targ the target variable
#' @export
#' @examples
#' bayes_fit(train.df,whole.df,"def")
bayes_fit<-function(train.df,whole.df,targ){
  vars<-names(train.df)
  vars<-vars[vars!=targ]
  bayes.fit<-list()
  palldef<-prop.table(table(train.df[,targ]))[2]
  pallgood<-1-palldef
  for(i in vars){
    probs<-as.character(train.df[,i])
    probs[is.na(probs)]<-'missing'
    wholeprobs<-as.character(whole.df[,i])
    wholeprobs[is.na(wholeprobs)]<-'missing'
    pv<-prop.table(table(wholeprobs))
    
    pbydef<-prop.table(table(probs,train.df[,targ]),2)[,2]
    pbydef<-pbydef[names(pv)]
    names(pbydef)<-names(pv)
    pbydef[is.na(pbydef)]<-0
    pbydef<-pbydef*palldef/pv
    
    pbygood<-prop.table(table(probs,train.df[,targ]),2)[,1]
    pbygood<-pbygood[names(pv)]
    names(pbygood)<-names(pv)
    pbygood[is.na(pbygood)]<-0
    pbygood<-pbygood*pallgood/pv
    temp<-pbydef/(pbydef+pbygood)
    temp[is.infinite(temp)]<-NA
    bayes.fit[[i]]<-temp
  }
  return(bayes.fit)
}

#' doing a bayes transformation 
#'
#' This function will doing a bayes transformation 
#'
#' @param df the dataframe 
#' @param bayes.fit fitted woe model
#' @export
#' @examples
#' woe_transform(p2p,woe.fit)
bayes_transform<-function(df,bayes.fit,hand.miss=T){
  for(i in names(bayes.fit)){
    if(!i%in%names(df))
      next
    vec<-as.character(df[,i])
    temp<-vec
    if(hand.miss)
      temp[is.na(temp)]<-'missing'
    bay<-bayes.fit[[i]]
    for(j in names(bay)){
      vec[temp==j]<-bay[j]
    }
    vec[!temp%in%names(bay)]<-NA
    df[,i]<-as.numeric(vec)
  }
  return(df)
}


