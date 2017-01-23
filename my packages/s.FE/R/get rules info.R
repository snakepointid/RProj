#' get the special rules
#'
#' get the special rules
#' 
#' @param df the dataframe
#' @param targ the target variable
#' @param min.odds the minimum bad odds level
#' @param min.num the minimum bad obs numbers

#' @export
#' @examples
#' 
#' get_rules(df,targ,min.odds,min.num)
get_rules<-function(df,targ){
  rulevars<-names(df)
  rulevars<-rulevars[rulevars!=targ]
  rule.list<-list()
  for(i in rulevars){
    bad.num<-table(df[,i],df[,targ])[,2]
    bad.odds<-prop.table(table(df[,i],df[,targ]),1)[,2]
    rule.list[[i]]<-rbind(bad.odds,bad.num)
  }
  return(rule.list)
}
#' get a rule hits variables
#'
#' get a rule hits variables
#' 
#' @param df the dataframe
#' @param rule.list the rule list


#' @export
#' @examples
#' get_rule_hit(df,rule.list)
get_rule_hit<-function(df,rule.list,min.odds,min.num){
  hit.vars<-names(rule.list)
  hit.vec<-c()
  for(i in hit.vars){
    temp.df<-rule.list[[i]]
    mp<-which(temp.df[1,]>min.odds&temp.df[2,]>min.num)
    value<-colnames(temp.df)[mp]
    if(length(value)>0){
     mp<-which(as.character(df[,i])%in%value)
     hit.vec<-c(hit.vec,mp)}
  }
  return(hit.vec)
}
#' find those vars which match the bisic var structures
#'
#' find those vars which match the bisic var structures
#' 
#' @param oris basic var structure
#' @param vars complate vars
#' @param l the place which don't care

#' @export
#' @examples
#' 
#'match_orisv(oris,vars,l)
match_oris<-function(oris,vars,l){
  comp<-unlist(str_extract_all(oris,"[a-z,A-Z,0-9]+"))
  loc<-1:length(comp)
  loc<-loc[-l]
  for(i in vars){
    compi<-unlist(str_extract_all(i,"[a-z,A-Z,0-9]+"))
    if(length(compi)!=length(comp))
      vars<-vars[vars!=i]else{
        for(j in loc){
          if(comp[j]!=compi[j])vars<-vars[vars!=i]
        }}
  }
  return(vars)
}
