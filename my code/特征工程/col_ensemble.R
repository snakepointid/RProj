
#' randomly put variables into different groups
#'
#' randomly put variables into different groups
#' 
#' @param cols those variables used to seperation
#' @param ngroups how many groups those variables will be
#' @export
#' @examples
#' 
#' get_col_list(useful,10)
get_col_list <- function(cols,ngroups) {
  set.seed(123)
  remain <- cols
  ncols <- round(length(cols) / ngroups)
  col.list <- list()
  for (i in 1:(ngroups - 1)) {
    temp <- sample(remain,ncols,replace = F)
    col.list[[i]] <- temp
    remain <- remain[!remain %in% temp]
  }
  col.list[[ngroups]] <- remain
  return(col.list)
}
#' every group will fit a submodel by gbdt
#'
#' every group will fit a submodel by gbdt
#'
#' @param df the dataframe
#' @param useful used features
#' @param targ the label variable
#' @param ngroups how many submodels those variables will be
#' @export
#' @examples
#' 
#' col_Ensemble_fit(df =p2p.model,useful,targ="def",ngroups=10)
col_Ensemble_fit <- function(df,useful,targ,ngroups) {
  set.seed(123)
  useful <- useful[useful != targ]
  col.list <- get_col_list(useful,ngroups)
  bst.list <- list()
  trans.df <- data.frame(df[,targ])
  for (i in 1:ngroups) {
    bst <- xg_model(df,col.list[[i]],targ)
    train.mat <- sparse.model.matrix( ~ ., data = df[,col.list[[i]]])
    prob <- predict(bst,newdata = train.mat)
    trans.df <- data.frame(trans.df,prob)
    bst.list[[i]] <- bst
  }
  formula <- as.formula(paste(names(trans.df)[1],"~."))
  logic.fit <-
    glm(formula,data = trans.df,family = binomial(link = "logit"))
  return(list(
    gbt = bst.list,logic = logic.fit,cols = col.list
  ))
}
#' ensemble those submodels' prediction
#'
#' ensemble those submodels' prediction
#' 
#' @param ndf the dataframe
#' @param fit.model ensemble fitted models by col_Ensemble_fit
#' @export
#' @examples
#' 
#' col_Ensemble_predict(ndf =p2p.model,fit.model)
col_Ensemble_predict <- function(ndf,fit.model) {
  bst.list <- fit.model[["gbt"]]
  log.fit <- fit.model[["logic"]]
  col.list <- fit.model[["cols"]]
  ngroups <- length(col.list)
  df <- data.frame(ndf[,1])
  for (i in 1:ngroups) {
    train.mat <- sparse.model.matrix( ~ ., data = ndf[,col.list[[i]]])
    prob <- predict(bst.list[[i]],newdata = train.mat)
    df <- data.frame(df,prob)
  }
  df <- df[,-1]
  prob <- predict(log.fit,newdata = df,type = "response")
  return(prob)
}